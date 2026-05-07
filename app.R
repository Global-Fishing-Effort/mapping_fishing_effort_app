# Fishing Effort Visualization Shiny App

# ---- Packages ----
packages <- c(
  "shiny", "ggplot2", "dplyr", "qs", "plotly", "sf", "rnaturalearth",
  "htmltools", "scales", "rnaturalearthdata", "shinycssloaders", "tidyr"
)

invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))

# Keep full error messages visible while developing/debugging the app.
# Set to TRUE for public deployment if you want to hide internal errors.
options(shiny.sanitize.errors = FALSE)

# ---- Shared constants and small helpers ----

MODEL_DATA_DIRS <- c(
  industrial = "rf_model_data_ind",
  artisanal  = "rf_model_data_art",
  combined   = "rf_model_data_combined"
)

INFO_BOX_STYLE <- paste(
  "background-color: #f8f9fa; border: 1px solid #dee2e6;",
  "border-radius: 5px; padding: 15px; margin-bottom: 20px;"
)

ROUSSEAU_BOX_STYLE <- paste(
  "background-color: #fff3cd; border: 1px solid #ffeaa7;",
  "border-radius: 5px; padding: 15px; margin-bottom: 20px;"
)

# Expanded palette for stacked area plots with many categories.
mypal <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5",
  "#c49c94", "#f7b6d3", "#c7c7c7", "#dbdb8d", "#9edae5",
  "#393b79", "#5254a3", "#6b6ecf", "#9c9ede", "#637939",
  "#8ca252", "#b5cf6b", "#cedb9c", "#8c6d31", "#bd9e39"
)

# Consistent binned colour scale for map panels.
EFFORT_BREAKS <- c(0.02, 0.2, 2, 20, 200, 2000)
EFFORT_LABELS <- c("<0.02", "0.02-0.2", "0.2-2", "2-20", "20-200", "200-2000", ">2000")
EFFORT_COLORS <- c("#F0F0F0", "#EFF3FE", "#CADBEE", "#A8C9E0", "#E8F4A2", "#F1B16D", "#C54B53")
names(EFFORT_COLORS) <- EFFORT_LABELS

# Build named choices for Shiny inputs, optionally prepending an "All" choice.
make_choices <- function(x, include_all = TRUE, all_label = "All") {
  vals <- sort(unique(stats::na.omit(x)))
  choices <- setNames(as.list(vals), vals)
  if (include_all) {
    choices <- c(setNames(list("All"), all_label), choices)
  }
  choices
}

# Return model column names for nominal/effective effort.
get_effort_columns <- function(effort_type) {
  switch(
    effort_type,
    nominal = list(
      hours = "nom_active_fishing_hours",
      days = "nom_active_fishing_days",
      total_hours = "total_nominal_fishing_hours"
    ),
    effective = list(
      hours = "eff_active_fishing_hours",
      days = "eff_active_fishing_days",
      total_hours = "total_effective_fishing_hours"
    )
  )
}

# Rousseau data use slightly different column names.
get_rousseau_effort_column <- function(effort_type) {
  switch(effort_type, nominal = "nom_active", effective = "eff_active")
}

# Text labels used in legends.
group_label <- function(group_var) {
  dplyr::case_when(
    group_var == "gear" ~ "Gear Type",
    group_var == "length_category" ~ "Vessel Length",
    group_var == "f_group" ~ "Functional Group",
    group_var == "sector" ~ "Sector",
    TRUE ~ group_var
  )
}

# Reusable status boxes at the top of tabs.
info_box <- function(title, ..., style = INFO_BOX_STYLE, color = "#495057", text_color = "#6c757d") {
  div(
    style = style,
    h4(title, style = paste0("margin-top: 0; color: ", color, ";")),
    p(..., style = paste0("margin-bottom: 0; color: ", text_color, ";"))
  )
}

# Add effort bins and ensure the bin factor always has the same legend order.
add_effort_bins <- function(df) {
  df %>%
    mutate(
      effort_bin = cut(
        effort_per_km2,
        breaks = c(-Inf, EFFORT_BREAKS, Inf),
        labels = EFFORT_LABELS,
        right = TRUE
      ),
      effort_bin = factor(effort_bin, levels = EFFORT_LABELS)
    )
}

# Empty map helper used when filters produce no rows.
empty_map <- function(world, label = "No effort for this query") {
  ggplot() +
    geom_sf(data = world, fill = "lightgray", color = "white", size = 0.2) +
    annotate("text", x = 0, y = 0, label = label, size = 6, color = "darkred", fontface = "bold") +
    coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +
    theme_bw() +
    labs(title = "Modelled Fishing Effort") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
}

# Shared stacked area plotting function for both modelled and Rousseau time series.
make_time_series_plot <- function(df, group_var, effort_type, title_suffix) {
  data_complete <- df %>%
    complete(year = full_seq(year, 1), !!sym(group_var), fill = list(total_effort = 0))
  
  group_order <- data_complete %>%
    group_by(!!sym(group_var)) %>%
    summarise(total = sum(total_effort, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total)) %>%
    pull(!!sym(group_var))
  
  data_complete[[group_var]] <- factor(data_complete[[group_var]], levels = group_order)
  
  year_breaks <- sort(unique(data_complete$year))
  year_labels <- ifelse(year_breaks %% 2 == 0 | year_breaks %in% range(year_breaks), year_breaks, "")
  effort_type_label <- if (effort_type == "nominal") "Nominal" else "Effective"
  
  ggplot(data_complete, aes(x = year, y = total_effort, fill = !!sym(group_var))) +
    geom_area(stat = "identity", alpha = 0.85, position = "stack") +
    scale_x_continuous(breaks = year_breaks, labels = year_labels) +
    scale_fill_manual(values = mypal) +
    theme_bw() +
    labs(
      title = paste(effort_type_label, "Fishing Effort Over Time -", title_suffix),
      y = "kW days",
      fill = group_label(group_var)
    ) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 12)
    )
}

# ---- Data loading ----
list_model_files <- function(data_dir) {
  list.files(data_dir, pattern = "model_preds_1950_2017_.*\\.qs$", full.names = TRUE)
}

read_model_file <- function(file_path) {
  qs::qread(file_path) %>%
    filter(nom_active_fishing_hours > 0)
}

read_model_dataset <- function(data_dir) {
  lapply(list_model_files(data_dir), read_model_file) %>%
    bind_rows()
}

# Spatial model outputs used by the map tab.
data_industrial <- read_model_dataset(MODEL_DATA_DIRS[["industrial"]])
data_artisanal  <- read_model_dataset(MODEL_DATA_DIRS[["artisanal"]])
data_combined   <- read_model_dataset(MODEL_DATA_DIRS[["combined"]])

spatial_data <- list(
  industrial = data_industrial,
  artisanal  = data_artisanal,
  combined   = data_combined
)

# Pre-aggregated time-series model outputs.
time_series_data <- list(
  industrial = qs::qread("timeseries_data/all_timeseries_data_grouped_ind.qs"),
  artisanal  = qs::qread("timeseries_data/all_timeseries_data_grouped_art.qs")
)

# Rousseau data metadata.
rousseau_files <- list.files("rousseau_data", pattern = ".*_effort\\.qs$", full.names = FALSE)
rousseau_countries <- gsub("_effort\\.qs$", "", rousseau_files)
country_names <- qs::qread("data/country_names_mapping.qs")

read_rousseau_data <- function(country_code) {
  tryCatch({
    file_path <- file.path("rousseau_data", paste0(country_code, "_effort.qs"))
    if (file.exists(file_path)) qs::qread(file_path) else NULL
  }, error = function(e) NULL)
}

read_rousseau_all_data <- function(group_var) {
  file_mapping <- c(
    gear = "All_gear_effort.qs",
    length_category = "All_length_effort.qs",
    f_group = "All_fgroup_effort.qs",
    sector = "All_sector_effort.qs"
  )
  
  tryCatch({
    if (!group_var %in% names(file_mapping)) return(NULL)
    file_name <- unname(file_mapping[group_var])
    file_path <- file.path("rousseau_data", "all_dfs", file_name)
    if (file.exists(file_path)) qs::qread(file_path) else NULL
  }, error = function(e) NULL)
}

# ---- UI helper components ----
sector_choices <- c("Industrial" = "industrial", "Artisanal" = "artisanal")
map_sector_choices <- c(sector_choices, "Combined (Industrial + Artisanal)" = "combined")
effort_choices <- c("Nominal" = "nominal", "Effective" = "effective")
location_choices <- c("EEZ" = "eez", "FAO Fishing Area" = "fao")

ui <- fluidPage(
  titlePanel(
    div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      img(src = "fishing_effort_logo.png", height = 250, width = 500),
      img(src = "IMAS_logo.png", height = 200, width = 400)
    ),
    windowTitle = "Global Fishing Effort Mapper"
  ),
  
  tabsetPanel(
    id = "tabset",
    
    # ---- Map tab ----
    tabPanel(
      "Map",
      sidebarLayout(
        sidebarPanel(
          radioButtons("map_sector", "Select Sector:", choices = 
                         map_sector_choices, selected = "industrial"),
          radioButtons("map_effort_type", "Select Effort Type:",
                       choices = effort_choices, selected = "nominal"),
          radioButtons(
            "map_group_var", "Group by:",
            choices = c("Gear Type" = "gear", "Vessel Length Category" = "length_category"),
            selected = "gear"
          ),
          uiOutput("map_gear_selector"),
          uiOutput("map_length_selector"),
          selectInput(
            "map_flag_country", "Select Flag Country (Fishing Fleet):",
            choices = make_choices(data_industrial$flag_country_name),
            selected = "All", multiple = TRUE
          ),
          radioButtons("map_location_type", "Select Location Type:", 
                       choices = location_choices, selected = "eez"),
          uiOutput("map_location_selector"),
          sliderInput("map_year", "Select Year:", min = 1950, max = 2017,
                      value = 2017, step = 1, sep = "", animate = TRUE),
          downloadButton("downloadMapData", "Download Data")
        ),
        mainPanel(
          info_box(
            "New Modeled Spatial Data",
            "This tab displays ", strong("new modeled fishing effort data"),
            " created using our latest random forest spatial allocation methodology.
            This represents our most current approach to mapping global fishing effort 
            using AIS data (industrial) and Sentinel-2/Skylight vessel detections 
            (artisanal). See the About tab for more information."
          ),
          plotOutput("map", height = "600px")
        )
      )
    ),
    
    # ---- Time Series tab ----
    tabPanel(
      "Time Series",
      sidebarLayout(
        sidebarPanel(
          radioButtons("sector", "Select Sector:", choices = sector_choices, 
                       selected = "industrial"),
          radioButtons("effort_type", "Select Effort Type:", choices = effort_choices, 
                       selected = "nominal"),
          radioButtons(
            "group_var", "Group by:",
            choices = c("Gear Type" = "gear", "Vessel Length Category" = "length_category"),
            selected = "gear"
          ),
          selectInput(
            "flag_country", "Select Flag Country (Fishing Fleet):",
            choices = make_choices(data_industrial$flag_country_name),
            selected = "All", multiple = TRUE
          ),
          radioButtons("location_type", "Select Location Type:", 
                       choices = location_choices, selected = "eez"),
          uiOutput("location_selector"),
          downloadButton("downloadData", "Download Data")
        ),
        mainPanel(
          info_box(
            "New Modeled Time Series Data",
            "This tab displays ", strong("new modeled fishing effort time series"),
            " created using our latest random forest spatial allocation methodology. 
            This represents our most current approach to mapping temporal trends 
            in global fishing effort using combined industrial and artisanal data. 
            See the About tab for more information."
          ),
          withSpinner(plotlyOutput("timeSeries", height = "500px"), 
                      caption = "This takes a few seconds to load")
        )
      )
    ),
    
    # ---- Rousseau tab ----
    tabPanel(
      "Rousseau et al. 2024 data",
      sidebarLayout(
        sidebarPanel(
          radioButtons("rousseau_sector", "Select Sector:", 
                       choices = sector_choices, selected = "industrial"),
          radioButtons("rousseau_effort_type", "Select Effort Type:", 
                       choices = effort_choices, selected = "nominal"),
          radioButtons(
            "rousseau_group_var", "Group by:",
            choices = c("Gear Type" = "gear", "Vessel Length Category" = "length_category", 
                        "Functional Group" = "f_group"),
            selected = "gear"
          ),
          selectInput("rousseau_flag_country", "Select Flag Country (Fishing Fleet):", 
                      choices = NULL, selected = NULL),
          radioButtons("rousseau_location_type", "Select Location Type:", 
                       choices = location_choices, selected = "eez"),
          uiOutput("rousseau_location_selector"),
          downloadButton("downloadRousseauData", "Download Data")
        ),
        mainPanel(
          info_box(
            "Rousseau et al. 2024 Dataset",
            "This tab displays ", strong("data from "),
            tags$a(href = "https://www.nature.com/articles/s41597-023-02824-6", 
                   "Rousseau et al. 2024", style = "color: #856404; text-decoration: underline;"),
            " representing a previous spatial allocation model and dataset. 
            This provides an independent comparison to the new modeled data 
            shown in the other tabs, using different methodological approaches 
            for mapping global fishing effort. See the About tab for more information.",
            style = ROUSSEAU_BOX_STYLE, color = "#856404", text_color = "#856404"
          ),
          withSpinner(plotlyOutput("rousseauTimeSeries", height = "500px"), 
                      caption = "This takes a few seconds to load")
        )
      )
    ),
    
    # ---- About tab ----
    tabPanel(
      "About",
      div(
        style = "max-width: 1000px; margin: 0 auto; padding: 20px;",
        h2("About this website", style = "text-align: center; margin-bottom: 20px;"),
        p("This app provides an interactive platform for exploring and downloading 
          mapped global fishing effort data. Users can filter by ", 
          strong("year, country, gear type, vessel length, sector, 
                 Exclusive Economic Zone (EEZ), and FAO statistical area"), " 
          using the selection sidebar in each tab."),
        p("This latest version of our mapping methodology integrates country-level 
          fishing effort estimates with a statistical spatial allocation model 
          using random forest modeling. The industrial model is built using 
          AIS-derived fishing activity from ", 
          tags$a(href = "https://globalfishingwatch.org/datasets-and-code/", 
                 "Global Fishing Watch"), ", while the artisanal model is built 
          using vessel detections from Sentinel-2 from ", 
          tags$a(href = "https://globalfishingwatch.org/datasets-and-code/", 
                 "Global Fishing Watch"), " and vessel detections provided by ",
          tags$a(href = "https://www.skylight.global/", "Skylight"), " 
          (via Minderoo Foundation). We combine environmental, economic, 
          and governance variables with the AIS and vessel detections to 
          predict fishing effort globally."),
        p("For each fishing country, we trained a ", 
          strong("two-stage hurdle random forest model"), 
          " to predict the spatial distribution of fishing effort:"),
        tags$ul(
          tags$li("The first stage predicts whether fishing occurs in each grid 
                  cell globally from 1950-2017."),
          tags$li("The second stage estimates the intensity of fishing effort in 
                  each cell globally from 1950-2017.")
        ),
        p("By multiplying the predictions from both stages, we obtain the 
          estimated fishing intensity (the proportion of a country's total fishing 
          effort) in each cell where fishing is predicted to occur. These estimates 
          are then scaled to ", strong("kW days of fishing effort"), " using total 
          fishing effort values (or number of vessels for artisanal) from ", 
          em(tags$a(href = "https://doi.org/10.1073/pnas.1820344116", "Rousseau et al. 2019")), 
          " (Figure 1)."),
        tags$hr(),
        img(src = "MS_fig1_new.png", height = "auto", width = "100%"),
        p(strong("Figure 1: "), "Schematic overview of the two-stage random 
          forest hurdle modelling and spatial allocation workflow. (A) Observed 
          inputs used to train models, including AIS apparent fishing effort for 
          industrial fleets (2015–2024) satellite-based vessel detections for artisanal 
          fleets (2009–2024) and environmental and governance predictors 
          (e.g., sea-surface temperature, chlorophyll, depth, distance to shore, 
          EEZ/FAO regions). (B) Two-stage hurdle models: Stage 1 predicts fishing 
          presence/absence, and Stage 2 predicts relative effort intensity (proportions), 
          with exclusion layers applied as appropriate (e.g., sea ice; industrial 
          fishing-access constraints; artisanal populated/coastal zones). 
          (C) Model predictions are then used to spatially allocate reported 
          global fishing effort totals to produce annual gridded fishing-effort 
          surfaces (1° × 1°; 1950–2017). Icons adapted from Canva.com."),
        p("Mapped effort estimates are provided as nominal fishing effort 
          (kilowatt days) or effective fishing effort (kilowatt days), 
          with a spatial resolution of 1° cell (industrial) and 0.5° cell 
          (artisanal), spanning the years 1950-2017. To estimate effective effort, 
          Rousseau et al. 2019 assumed a year-on-year increase in technical efficiency of 3.5%, 
          as in Rousseau et al. 2019."),
        p("This app and the underlying data was created, and is under continuous 
          development by Gage Clawson, Camilla Novaglio & Julia Blanchard from the 
          Institute for Marine & Antarctic Studies (IMAS), University of Tasmania."),
        
        h3("Caveats and limitations", style = "margin-top: 30px"),
        p("This data is likely not comprehensive and represents modeled outputs 
          only. For example, estimates in Southeast Asia, aside from China, are 
          likely too concentrated for some years (e.g., Indonesia). This is an 
          artifact of insufficient AIS data in this region."),
        
        h3("Rousseau et al. 2024 Data Tab", style = "margin-top: 30px;"),
        p("The ", strong("'Rousseau et al. 2024 data' tab"), " provides access 
          to an independent dataset of mapped global fishing activity from ", 
          tags$a(href = "https://www.nature.com/articles/s41597-023-02824-6", 
                 "Rousseau et al. 2024"), ": \"A database of mapped global 
          fishing activity 1950–2017\". This dataset represents a previous 
          spatial allocation of fishing effort data that is different from the 
          modeled estimates shown in the other tabs."),
        p("The Rousseau et al. 2024 dataset offers additional grouping options 
          including functional groups, allowing for detailed analysis of fishing 
          patterns by different fleet characteristics. Users can explore this data 
          by individual flag country, with the same temporal coverage (1950-2017) 
          and location filtering options (EEZ and FAO areas) as the other tabs. 
          Unfortunately, the mapped data (with latitude and longitude points) for 
          the Rousseau data is too large to add to this shiny app, however, this 
          data can be accessed via the IMAS data portal here: 
          https://data.imas.utas.edu.au/attachments/1241a51d-c8c2-4432-aa68-3d2bae142794/"),
        
        h3("How should I use this tool?", style = "margin-top: 30px;"),
        p("This app has three tabs that allow you to visualise and download fishing effort data:"),
        tags$ul(
          tags$li(strong("The 'Map' tab"), 
                  " allows you to explore spatially explicit industrial effort 
                  data globally and for a selected region (EEZ or FAO statistical area). 
                  You can also specify the year (between 1950 and 2017), flag country, 
                  sector, gear type, and vessel length category."),
          tags$li(strong("The 'Time series' tab"), " gives you the same options 
                  but allows you to explore trends in fishing effort across both 
                  industrial and artisanal sectors combined."),
          tags$li(strong("The 'Rousseau et al. 2024 data' tab"), " provides 
                  access to an independent dataset with additional grouping 
                  options including functional groups and sectors, allowing for 
                  detailed country-specific analysis.")
        ),
        
        h3("How should I cite data from this site?", style = "margin-top: 30px;"),
        p("You can download the data used to create the plots shown in this interactive 
          tool using the 'Download' button included under each tab. Additionally, ]
          all model data is available via Zenodo and our GitHub repository. As a 
          condition of this tool to access data, you must cite its use: Clawson, 
          S.G., Novaglio, C., & Blanchard J.L. (2025). Global Fishing Effort Model
          Data and Shiny App: ", tags$a(href = "https://zenodo.org/records/19600603", 
                                        "10.5281/zenodo.15110744.")),
        
        h3("How can I contact you?", style = "margin-top: 30px;"),
        p("If you have any ideas on how to improve this app or if you found any 
          issues, you can \"create an issue\" in our ", 
          tags$a(href = "https://github.com/Global-Fishing-Effort/mapping_fishing_effort_app", 
                 "GitHub repository.")),
        p("For general enquiry we can contact Julia Blanchard at ", 
          tags$a("julia.blanchard@utas.edu.au", href = "mailto:julia.blanchard@utas.edu.au")),
        
        h3("Acknowledgments", style = "margin-top: 30px;"),
        p("The development of this app was funded by the Food and 
          Agriculture Organization of the United Nations (FAO) and the Minderoo 
          Foundation. We would also like to acknowledge the use of computing 
          facilities provided by Digital Research Services, IT Services at the 
          University of Tasmania."),
        br()
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Store previous location selections so they can survive UI rebuilds when filters change.
  previous_location <- reactiveVal("All")
  previous_map_location <- reactiveVal("All")
  previous_rousseau_location <- reactiveVal("All")
  
  # Keep multi-select inputs intuitive: "All" is mutually exclusive with specific choices.
  enforce_all_selection <- function(input_id, previous_value = reactiveVal("All")) {
    observeEvent(input[[input_id]], {
      vals <- input[[input_id]]
      if (is.null(vals) || length(vals) == 0) return()
      
      old_vals <- previous_value()
      selected <- vals
      
      if ("All" %in% vals && length(vals) > 1) {
        # If All was already selected, assume the user just added a specific item.
        # Otherwise, assume the user just selected All and clear the specific items.
        selected <- if ("All" %in% old_vals) setdiff(vals, "All") else "All"
        updateSelectInput(session, input_id, selected = selected)
      }
      
      previous_value(selected)
    }, ignoreNULL = FALSE)
  }
  
  enforce_all_selection("flag_country")
  enforce_all_selection("map_flag_country")
  enforce_all_selection("location_selection", previous_location)
  enforce_all_selection("map_location_selection", previous_map_location)
  enforce_all_selection("rousseau_location_selection", previous_rousseau_location)
  
  # Current datasets selected by sector.
  current_spatial_data <- reactive(spatial_data[[input$sector]])
  current_time_series_data <- reactive(time_series_data[[input$sector]])
  current_map_data <- reactive(spatial_data[[input$map_sector]])
  
  # Update time-series controls when switching between industrial and artisanal data.
  observeEvent(input$sector, {
    df <- current_spatial_data()
    updateSelectInput(session, "flag_country", choices = make_choices(df$flag_country_name), selected = "All")
    
    if (input$sector == "industrial") {
      updateRadioButtons(
        session, "group_var",
        choices = c("Gear Type" = "gear", "Vessel Length Category" = "length_category"),
        selected = "gear"
      )
    } else {
      updateRadioButtons(
        session, "group_var",
        choices = c("Vessel Length Category" = "length_category"),
        selected = "length_category"
      )
    }
  }, ignoreInit = TRUE)
  
  # Update map controls when switching sector. Combined and artisanal data do not support gear panels.
  observeEvent(input$map_sector, {
    df <- current_map_data()
    updateSelectInput(session, "map_flag_country", choices = make_choices(df$flag_country_name), selected = "All")
    
    if (input$map_sector == "industrial") {
      updateRadioButtons(
        session, "map_group_var",
        choices = c("Gear Type" = "gear", "Vessel Length Category" = "length_category"),
        selected = "gear"
      )
    } else {
      updateRadioButtons(
        session, "map_group_var",
        choices = c("Vessel Length Category" = "length_category"),
        selected = "length_category"
      )
    }
  }, ignoreInit = TRUE)
  
  # Build location choices after filtering by flag country.
  location_ui <- function(input_id, df, selected_flags, location_type, previous_value = "All") {
    if (!"All" %in% selected_flags) {
      df <- df %>% filter(flag_country_name %in% selected_flags)
    }
    
    location_col <- if (location_type == "eez") "eez_sovereign_name" else 
      "fao_major_fishing_area"
    location_label <- if (location_type == "eez") "Select EEZ (Fishing Location):" 
    else "Select FAO Area (Fishing Location):"
    choices <- make_choices(df[[location_col]])
    selected <- intersect(previous_value, names(choices))
    if (length(selected) == 0) selected <- "All"
    
    selectInput(input_id, location_label, choices = choices, selected = selected, multiple = TRUE)
  }
  
  output$location_selector <- renderUI({
    req(input$flag_country, input$location_type)
    location_ui(
      input_id = "location_selection",
      df = current_spatial_data(),
      selected_flags = input$flag_country,
      location_type = input$location_type,
      previous_value = previous_location()
    )
  })
  
  output$map_location_selector <- renderUI({
    req(input$map_flag_country, input$map_location_type)
    location_ui(
      input_id = "map_location_selection",
      df = current_map_data(),
      selected_flags = input$map_flag_country,
      location_type = input$map_location_type,
      previous_value = previous_map_location()
    )
  })
  
  output$map_gear_selector <- renderUI({
    req(input$map_sector, input$map_group_var)
    if (input$map_sector != "industrial" || input$map_group_var != "gear") return(NULL)
    
    selectInput(
      "map_gear", "Select Gear Type:",
      choices = c("All aggregated" = "All_aggregated", make_choices(current_map_data()$gear, include_all = TRUE)),
      selected = "All_aggregated"
    )
  })
  
  output$map_length_selector <- renderUI({
    req(input$map_group_var)
    if (input$map_group_var != "length_category") return(NULL)
    
    selectInput(
      "map_length", "Select Vessel Length Category:",
      choices = c("All aggregated" = "All_aggregated", make_choices(current_map_data()$length_category, include_all = TRUE)),
      selected = "All_aggregated"
    )
  })
  
  # ---- Modelled time-series data ----
  filtered_time_series_data <- reactive({
    req(input$flag_country, input$location_selection, input$location_type)
    
    df <- current_time_series_data()
    
    if (!"All" %in% input$flag_country) {
      df <- df %>% filter(flag_country_name %in% input$flag_country)
    }
    
    if (!"All" %in% input$location_selection) {
      location_col <- if (input$location_type == "eez") "eez_sovereign_name" else "fao_major_fishing_area"
      df <- df %>% filter(.data[[location_col]] %in% input$location_selection)
    }
    
    df
  })
  
  aggregated_time_series_data <- reactive({
    req(filtered_time_series_data(), input$effort_type, input$group_var)
    effort_cols <- get_effort_columns(input$effort_type)
    
    filtered_time_series_data() %>%
      group_by(year, !!sym(input$group_var)) %>%
      summarise(total_effort = sum(!!sym(effort_cols$days), na.rm = TRUE), .groups = "drop")
  })
  
  output$timeSeries <- renderPlotly({
    req(aggregated_time_series_data(), input$effort_type, input$group_var)
    
    p <- make_time_series_plot(
      df = aggregated_time_series_data(),
      group_var = input$group_var,
      effort_type = input$effort_type,
      title_suffix = "Modeled"
    )
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # ---- Modelled map data ----
  filtered_map_data <- reactive({
    req(input$map_flag_country, input$map_location_selection, input$map_location_type, input$map_year, input$map_group_var)
    
    df <- current_map_data()
    
    if (!"All" %in% input$map_flag_country) {
      df <- df %>% filter(flag_country_name %in% input$map_flag_country)
    }
    
    if (!"All" %in% input$map_location_selection) {
      location_col <- if (input$map_location_type == "eez") "eez_sovereign_name" else "fao_major_fishing_area"
      df <- df %>% filter(.data[[location_col]] %in% input$map_location_selection)
    }
    
    # Apply the selected category only when the user chooses one specific category.
    if (input$map_sector == "industrial" && input$map_group_var == "gear" && !is.null(input$map_gear) 
        && !input$map_gear %in% c("All", "All_aggregated")) {
      df <- df %>% filter(gear == input$map_gear)
    }
    
    if (input$map_group_var == "length_category" && !is.null(input$map_length) && !input$map_length %in% 
        c("All", "All_aggregated")) {
      df <- df %>% filter(length_category == input$map_length)
    }
    
    df %>% filter(year == input$map_year)
  })
  
  output$map <- renderPlot({
    req(filtered_map_data(), input$map_effort_type, input$map_group_var)
    
    effort_cols <- get_effort_columns(input$map_effort_type)
    progress <- shiny::Progress$new()
    progress$set(message = "Rendering map...", value = 0)
    on.exit(progress$close())
    
    tryCatch({
      valid_data <- filtered_map_data() %>% filter(!is.na(lon), !is.na(lat))
      if (nrow(valid_data) == 0) return(empty_map(world))
      
      # Use the native display resolution for each sector.
      cell_size <- if (input$map_sector == "artisanal") 0.5 else 1
      cell_offset <- cell_size / 2
      
      raster_data <- valid_data %>%
        mutate(
          lon_bin = floor(lon / cell_size) * cell_size + cell_offset,
          lat_bin = floor(lat / cell_size) * cell_size + cell_offset
        )
      
      group_var <- input$map_group_var
      show_facets <- (group_var == "gear" && identical(input$map_gear, "All")) ||
        (group_var == "length_category" && identical(input$map_length, "All"))
      
      grouping_cols <- if (show_facets) c("lon_bin", "lat_bin", group_var) else c("lon_bin", "lat_bin")
      
      raster_data <- raster_data %>%
        group_by(across(all_of(grouping_cols))) %>%
        summarise(
          total_effort = sum(!!sym(effort_cols$hours), na.rm = TRUE),
          pixel_area_km2 = mean(pixel_area_km2, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(effort_per_km2 = total_effort / pixel_area_km2) %>%
        add_effort_bins()
      
      if (nrow(raster_data) == 0 || sum(raster_data$total_effort, na.rm = TRUE) == 0) {
        return(empty_map(world))
      }
      
      p <- ggplot() +
        geom_sf(data = world, fill = "lightgray", color = "white", size = 0.2) +
        geom_tile(
          data = raster_data,
          aes(x = lon_bin, y = lat_bin, fill = effort_bin),
          width = cell_size,
          height = cell_size,
          alpha = 0.7
        ) +
        coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +
        scale_fill_manual(
          name = "Fishing Effort (kW hours/km²)",
          values = EFFORT_COLORS,
          na.value = "transparent",
          drop = FALSE
        ) +
        guides(
          fill = guide_legend(
            title.position = "top",
            title.hjust = 0.5,
            nrow = 1,
            label.position = "bottom"
          )
        ) +
        labs(title = "Modelled Fishing Effort") +
        theme_bw() +
        theme(
          legend.position = "bottom",
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 10),
          strip.text = element_text(size = 10, face = "bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
      
      if (show_facets) {
        p <- p + facet_wrap(as.formula(paste("~", group_var)), ncol = 3)
      }
      
      p
    }, error = function(e) {
      print(paste("Error rendering map:", e$message))
      ggplot() +
        geom_sf(data = world, fill = "lightgray", color = "white", size = 0.2) +
        theme_bw() +
        labs(title = "Error rendering map", subtitle = "Check console for details")
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      flag_countries <- paste(input$flag_country, collapse = "_")
      location_type <- ifelse(input$location_type == "eez", "EEZ", "FAO")
      paste0("fishing_effort_data_", flag_countries, "_", location_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      filtered_time_series_data() %>%
        mutate(data_source = "Clawson et al. in prep") %>%
        write.csv(file, row.names = FALSE)
    }
  )
  
  output$downloadMapData <- downloadHandler(
    filename = function() {
      flag_countries <- paste(input$map_flag_country, collapse = "_")
      location_type <- ifelse(input$map_location_type == "eez", "EEZ", "FAO")
      paste0("fishing_effort_data_", flag_countries, "_", location_type, "_", input$map_year, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      filtered_map_data() %>%
        mutate(data_source = "Clawson et al. in prep") %>%
        write.csv(file, row.names = FALSE)
    }
  )
  
  # ---- Rousseau tab ----
  observe({
    available_countries <- intersect(rousseau_countries, names(country_names))
    country_choices <- setNames(as.list(available_countries), country_names[available_countries])
    country_choices <- country_choices[order(names(country_choices))]
    country_choices <- c("All" = "All", country_choices)
    
    updateSelectInput(session, "rousseau_flag_country", choices = country_choices, selected = "All")
  })
  
  rousseau_data <- reactive({
    req(input$rousseau_flag_country, input$rousseau_group_var, input$rousseau_sector)
    
    df <- if (input$rousseau_flag_country == "All") {
      read_rousseau_all_data(input$rousseau_group_var)
    } else {
      read_rousseau_data(input$rousseau_flag_country)
    }
    
    if (is.null(df)) return(NULL)
    if (!"sector" %in% names(df)) {
      warning("Rousseau data does not contain a 'sector' column")
      return(NULL)
    }
    
    df <- if (input$rousseau_sector == "industrial") {
      df %>% filter(sector == "Industrial")
    } else {
      df %>% filter(sector %in% c("Artisanal Powered", "Artisanal Unpowered"))
    }
    
    df %>% filter(nom_active > 0 | eff_active > 0)
  })
  
  output$rousseau_location_selector <- renderUI({
    req(input$rousseau_location_type, rousseau_data())
    df <- rousseau_data()
    if (is.null(df)) return(NULL)
    
    location_col <- if (input$rousseau_location_type == "eez") "eez_sovereign_name" 
    else "fao_major_fishing_area"
    location_label <- if (input$rousseau_location_type == "eez") "Select EEZ (Fishing Location):" 
    else "Select FAO Area (Fishing Location):"
    choices <- make_choices(df[[location_col]])
    selected <- intersect(previous_rousseau_location(), names(choices))
    if (length(selected) == 0) selected <- "All"
    
    selectInput("rousseau_location_selection", location_label, choices = choices, selected = selected, multiple = TRUE)
  })
  
  filtered_rousseau_data <- reactive({
    req(input$rousseau_location_selection, input$rousseau_location_type)
    df <- rousseau_data()
    if (is.null(df)) return(NULL)
    
    if (!"All" %in% input$rousseau_location_selection) {
      location_col <- if (input$rousseau_location_type == "eez") "eez_sovereign_name" else "fao_major_fishing_area"
      df <- df %>% filter(.data[[location_col]] %in% input$rousseau_location_selection)
    }
    
    df
  })
  
  aggregated_rousseau_data <- reactive({
    req(filtered_rousseau_data(), input$rousseau_effort_type, input$rousseau_group_var)
    df <- filtered_rousseau_data()
    if (is.null(df)) return(NULL)
    
    effort_col <- get_rousseau_effort_column(input$rousseau_effort_type)
    
    df %>%
      group_by(year, !!sym(input$rousseau_group_var)) %>%
      summarise(total_effort = sum(!!sym(effort_col), na.rm = TRUE), .groups = "drop")
  })
  
  output$rousseauTimeSeries <- renderPlotly({
    req(input$rousseau_effort_type, input$rousseau_group_var)
    
    df <- aggregated_rousseau_data()
    if (is.null(df) || nrow(df) == 0) {
      return(
        plot_ly() %>%
          layout(
            title = list(text = "No data available for the selected filters", x = 0.5, xanchor = "center"),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }
    
    p <- make_time_series_plot(
      df = df,
      group_var = input$rousseau_group_var,
      effort_type = input$rousseau_effort_type,
      title_suffix = "Rousseau et al. 2024"
    )
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  output$downloadRousseauData <- downloadHandler(
    filename = function() {
      location_type <- ifelse(input$rousseau_location_type == "eez", "EEZ", "FAO")
      paste0("rousseau_fishing_effort_data_", input$rousseau_flag_country, "_", location_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- filtered_rousseau_data()
      if (!is.null(df)) {
        df %>%
          mutate(data_source = "Rousseau et al. 2024") %>%
          write.csv(file, row.names = FALSE)
      }
    }
  )
}

# ---- Run app ----
shinyApp(ui = ui, server = server)
