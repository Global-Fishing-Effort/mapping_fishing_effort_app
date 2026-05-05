# Fishing Effort Visualization Shiny App
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(qs))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(sf))  # For spatial data handling
suppressPackageStartupMessages(library(rnaturalearth))  # For world map data
suppressPackageStartupMessages(library(htmltools))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(rnaturalearthdata))
suppressPackageStartupMessages(library(shinycssloaders))  # For loading spinners
suppressPackageStartupMessages(library(tidyr))

options(shiny.sanitize.errors = FALSE)

# Read all industrial data files
data_files_ind <- list.files("rf_model_data_ind", pattern = "model_preds_1950_2017_.*\\.qs$", full.names = TRUE)

# Read all artisanal data files
data_files_art <- list.files("rf_model_data_art", pattern = "model_preds_1950_2017_.*\\.qs$", full.names = TRUE)

# Function to read and process each file
read_data_file <- function(file_path) {
  # Extract the flag country code from the filename
  flag_code <- gsub(".*model_preds_(.+)\\.qs$", "\\1", file_path)
  
  # Read the data
  df <- qread(file_path) %>%
    filter(nom_active_fishing_hours > 0)
  
  # Return the data
  return(df)
}

# Read and combine all industrial data files
data_industrial <- lapply(data_files_ind, read_data_file) %>%
  bind_rows()

# Read and combine all artisanal data files
data_artisanal <- lapply(data_files_art, read_data_file) %>%
  bind_rows()

# For backward compatibility, keep 'data' as industrial data initially
data <- data_industrial

# Read timeseries data for both sectors
data_timeseries_ind <- qs::qread("timeseries_data/all_timeseries_data_grouped_ind.qs")
data_timeseries_art <- qs::qread("timeseries_data/all_timeseries_data_grouped_art.qs")

# # For backward compatibility, keep 'data_timeseries' as industrial data initially
# data_timeseries <- data_timeseries_ind

# Get list of available Rousseau data files (country codes)
rousseau_files <- list.files("rousseau_data", pattern = ".*_effort\\.qs$", full.names = FALSE)
rousseau_countries <- gsub("_effort\\.qs$", "", rousseau_files)

# Load country names mapping from external file
country_names <- qs::qread("data/country_names_mapping.qs")

# Function to read Rousseau data for a specific country
read_rousseau_data <- function(country_code) {
  tryCatch({
    file_path <- file.path("rousseau_data", paste0(country_code, "_effort.qs"))
    if (file.exists(file_path)) {
      return(qs::qread(file_path))
    }
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

All_cat_rousseau <- list.files("rousseau_data/all_dfs", pattern = ".*_effort\\.qs$", full.names = FALSE)


# UI
ui <- fluidPage(
  titlePanel(
    div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      img(src = "fishing_effort_logo.png", height = 250, width = 500),
      img(src = "IMAS_logo.png", height = 200, width = 400)
    ),
    windowTitle = "Global Fishing Effort Mapper"
  ),
  
  # Create a tabsetPanel at the top level
  tabsetPanel(id = "tabset",
              # Map Tab
              tabPanel("Map",
                       sidebarLayout(
                         sidebarPanel(
                           radioButtons("map_sector", "Select Sector:",
                                        choices = c("Industrial" = "industrial", 
                                                    "Artisanal" = "artisanal"),
                                        selected = "industrial"),
                           
                           radioButtons("map_effort_type", "Select Effort Type:",
                                        choices = c("Nominal" = "nominal", 
                                                    "Effective" = "effective"),
                                        selected = "nominal"),
                           
                           radioButtons("map_group_var", "Group by:",
                                        choices = c("Gear Type" = "gear", 
                                                    "Vessel Length Category" = "length_category"),
                                        selected = "gear"),
                           
                           uiOutput("map_gear_selector"),
                           uiOutput("map_length_selector"),
                           
                           selectInput("map_flag_country", "Select Flag Country (Fishing Fleet):",
                                       choices = c("All" = "All", setNames(as.list(sort(unique(data$flag_country_name))), sort(unique(data$flag_country_name)))),
                                       selected = "All",
                                       multiple = TRUE),
                           
                           # Add JavaScript to handle the selection logic for map flag country
                           tags$script(HTML("
            $(document).ready(function() {
              // Wait for Shiny to initialize the input
              setTimeout(function() {
                // Get the selectize instance
                var $select = $('#map_flag_country').selectize();
                var selectize = $select[0].selectize;
                
                // Function to handle selection changes
                selectize.on('item_add', function(value) {
                  if (value === 'All') {
                    // If 'All' is selected, remove all other selections
                    var currentItems = selectize.items.slice();
                    currentItems.forEach(function(item) {
                      if (item !== 'All') {
                        selectize.removeItem(item);
                      }
                    });
                  } else {
                    // If any other item is selected, remove 'All'
                    if (selectize.items.includes('All')) {
                      selectize.removeItem('All');
                    }
                  }
                });
              }, 500); // Wait 500ms for Shiny to initialize
            });
          ")),
                           
                           radioButtons("map_location_type", "Select Location Type:",
                                        choices = c("EEZ" = "eez", 
                                                    "FAO Fishing Area" = "fao"),
                                        selected = "eez"),
                           
                           uiOutput("map_location_selector"),
                           
                           sliderInput("map_year", "Select Year:",
                                       min = 1950, 
                                       max = 2017,
                                       value = 2017,
                                       step = 1,
                                       sep = "",
                                       animate = TRUE),
                           
                           # Download button for Map tab
                           downloadButton("downloadMapData", "Download Data")
                         ),
                         
                         mainPanel(
                           div(
                             style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; margin-bottom: 20px;",
                             h4("New Modeled Spatial Data", style = "margin-top: 0; color: #495057;"),
                             p("This tab displays ", strong("new modeled fishing effort data"), 
                               " created using our latest random forest spatial allocation methodology. 
                               This represents our most current approach to mapping global fishing effort 
                               using AIS data (industrial) and Sentinel-2/Skylight vessel detections (artisanal).
                               See the About tab for more information.",
                               style = "margin-bottom: 0; color: #6c757d;")
                           ),
                           plotOutput("map", height = "600px")
                         )
                       )
              ),
              
              # Time Series Tab
              tabPanel("Time Series",
                       sidebarLayout(
                         sidebarPanel(
                           radioButtons("sector", "Select Sector:",
                                        choices = c("Industrial" = "industrial", 
                                                    "Artisanal" = "artisanal"),
                                        selected = "industrial"),
                           
                           radioButtons("effort_type", "Select Effort Type:",
                                        choices = c("Nominal" = "nominal", 
                                                    "Effective" = "effective"),
                                        selected = "nominal"),
                           
                           radioButtons("group_var", "Group by:",
                                        choices = c("Gear Type" = "gear", 
                                                    "Vessel Length Category" = "length_category"),
                                        selected = "gear"),
                           
                           
                           selectInput("flag_country", "Select Flag Country (Fishing Fleet):",
                                       choices = c("All" = "All", setNames(as.list(sort(unique(data$flag_country_name))), sort(unique(data$flag_country_name)))),
                                       selected = "All",
                                       multiple = TRUE),
                           
                           # Add JavaScript to handle the selection logic for flag country
                           tags$script(HTML("
            $(document).ready(function() {
              // Wait for Shiny to initialize the input
              setTimeout(function() {
                // Get the selectize instance
                var $select = $('#flag_country').selectize();
                var selectize = $select[0].selectize;
                
                // Function to handle selection changes
                selectize.on('item_add', function(value) {
                  if (value === 'All') {
                    // If 'All' is selected, remove all other selections
                    var currentItems = selectize.items.slice();
                    currentItems.forEach(function(item) {
                      if (item !== 'All') {
                        selectize.removeItem(item);
                      }
                    });
                  } else {
                    // If any other item is selected, remove 'All'
                    if (selectize.items.includes('All')) {
                      selectize.removeItem('All');
                    }
                  }
                });
              }, 500); // Wait 500ms for Shiny to initialize
            });
          ")),
                           
                           radioButtons("location_type", "Select Location Type:",
                                        choices = c("EEZ" = "eez", 
                                                    "FAO Fishing Area" = "fao"),
                                        selected = "eez"),
                           
                           uiOutput("location_selector"),
                           
                           # Download button for Time Series tab
                           downloadButton("downloadData", "Download Data")
                         ),
                         
                         mainPanel(
                           div(
                             style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; margin-bottom: 20px;",
                             h4("New Modeled Time Series Data", style = "margin-top: 0; color: #495057;"),
                             p("This tab displays ", strong("new modeled fishing effort time series"), 
                               " created using our latest random forest spatial allocation methodology. 
                               This represents our most current approach to mapping temporal trends in 
                               global fishing effort using combined industrial and artisanal data. 
                               See the About tab for more information.",
                               style = "margin-bottom: 0; color: #6c757d;")
                           ),
                           withSpinner(plotlyOutput("timeSeries", height = 
                                                      "500px"), caption = 
                                         "This takes a few seconds to load")
                         )
                       )
              ),
              
              # Rousseau et al. 2024 data Tab
              tabPanel("Rousseau et al. 2024 data",
                       sidebarLayout(
                         sidebarPanel(
                           radioButtons("rousseau_effort_type", "Select Effort Type:",
                                        choices = c("Nominal" = "nominal", 
                                                    "Effective" = "effective"),
                                        selected = "nominal"),
                           
                           radioButtons("rousseau_group_var", "Group by:",
                                        choices = c("Gear Type" = "gear", 
                                                    "Vessel Length Category" = "length_category",
                                                    "Functional Group" = "f_group",
                                                    "Sector" = "sector"),
                                        selected = "gear"),
                           
                           selectInput("rousseau_flag_country", "Select Flag Country (Fishing Fleet):",
                                       choices = NULL,
                                       selected = NULL,
                                       multiple = FALSE),
                           
                           radioButtons("rousseau_location_type", "Select Location Type:",
                                        choices = c("EEZ" = "eez", 
                                                    "FAO Fishing Area" = "fao"),
                                        selected = "eez"),
                           
                           uiOutput("rousseau_location_selector"),
                           
                           # Download button for Rousseau tab
                           downloadButton("downloadRousseauData", "Download Data")
                         ),
                         
                         mainPanel(
                           div(
                             style = "background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 5px; padding: 15px; margin-bottom: 20px;",
                             h4("Rousseau et al. 2024 Dataset", style = "margin-top: 0; color: #856404;"),
                             p("This tab displays ", strong("data from "), 
                               tags$a(href = "https://www.nature.com/articles/s41597-023-02824-6", 
                                      "Rousseau et al. 2024", style = "color: #856404; text-decoration: underline;"), 
                               " representing a previous spatial allocation model and dataset. 
                               This provides an independent comparison to the new modeled data 
                               shown in the other tabs, using different methodological approaches 
                               for mapping global fishing effort. See the About tab for more information.",
                               style = "margin-bottom: 0; color: #856404;")
                           ),
                           withSpinner(plotlyOutput("rousseauTimeSeries", height = 
                                                      "500px"), caption = 
                                         "This takes a few seconds to load")
                         )
                       )
              ),
              
              # About Tab (full width)
              tabPanel("About",
                       div(
                         style = "max-width: 1000px; margin: 0 auto; padding: 20px;",
                         h2("About this website", style = "text-align: center; margin-bottom: 20px;"),
                         p("This app provides an interactive platform for
                         exploring and downloading mapped global 
                         fishing effort data. 
                          Users can filter by ", strong("year, country, gear 
                                                        type, vessel length, sector,
                                                        Exclusive Economic Zone 
                                                        (EEZ), and FAO 
                                                        statistical area"), 
                           " using the selection sidebar in each tab."),
                         
                         p("This latest version of our mapping methodology 
                           integrates country-level fishing effort estimates with 
                           a statistical spatial allocation model using random forest modeling. 
                           The industrial model is built using AIS-derived fishing activity 
                           from ", tags$a(href = "https://globalfishingwatch.org/datasets-and-code/", "Global 
                           Fishing Watch"), ", while the artisanal model is built using 
                           vessel detections from Sentinel-2 from ", 
                           tags$a(href = "https://globalfishingwatch.org/datasets-and-code/", "Global 
                           Fishing Watch"), " and vessel detections provided by ", 
                           tags$a(href = "https://www.skylight.global/", "Skylight"), 
                           " (via Minderoo Foundation). We combine environmental, economic, 
                           and governance variables with the AIS and vessel detections 
                           to predict fishing effort globally."),
                         
                         p("For each fishing country, we trained a ", 
                           strong("two-stage hurdle random forest model"), 
                           " to predict the spatial distribution of fishing effort:"),
                         
                         tags$ul(
                           tags$li("The first stage predicts whether fishing 
                                   occurs in each grid cell globally from 1950-2017."),
                           tags$li("The second stage estimates the intensity 
                                   of fishing effort in each cell globally from 1950-2017.")
                         ),
                         
                         p("By multiplying the predictions from both stages, we
                         obtain the estimated fishing intensity 
                             (the proportion of a country's total fishing effort
                             ) in each cell where fishing is predicted to occur. 
                             These estimates are then scaled to ", strong("kW days of fishing effort"), 
                           "using total fishing effort values (or number of vessels for artisanal) from ", 
                           em(tags$a(href = "https://doi.org/10.1073/pnas.1820344116", 
                                     "Rousseau et al. 2019")), " (Figure 1)."),
                         # Adding Figure 1 below the text
                         tags$hr(),  # Horizontal line for separation
                         img(src = "MS_fig1_new.png", height = "auto", width = "100%"),
                         p(strong("Figure 1: "), "Schematic overview of the two-
                           stage random forest hurdle modelling and spatial 
                           allocation workflow. (A) Observed inputs used to 
                           train models, including AIS apparent fishing effort 
                           for industrial fleets (2015–2024) satellite-based 
                           vessel detections for artisanal fleets (2009–2024) 
                           and environmental and governance predictors (e.g., 
                           sea-surface temperature, chlorophyll, depth, 
                           distance to shore, EEZ/FAO regions). (B) Two-stage 
                           hurdle models: Stage 1 predicts fishing 
                           presence/absence, and Stage 2 predicts relative 
                           effort intensity (proportions), with exclusion 
                           layers applied as appropriate (e.g., sea ice; 
                           industrial fishing-access constraints; artisanal 
                           populated/coastal zones). (C) Model predictions are 
                           then used to spatially allocate reported global 
                           fishing effort totals to produce annual gridded 
                           fishing-effort surfaces (1° × 1°; 1950–2017). 
                           Icons adapted from Canva.com.  "),
                         
                         p("Mapped effort estimates are provided as nominal 
                           fishing effort (kilowatt days) or effective fishing 
                           effort (kilowatt days), with a spatial resolution of 
                           1° cell (industrial) and 0.5° cell (artisanal), 
                           spanning the years 1950-2017. To estimate 
                           effective effort, Rousseau et al. 2019 have assumed a year-on-year 
                           increase in technical efficiency of 3.5%, as in 
                           Rousseau et al. 2019."),
                         
                         p("This app and the underlying data was created, and is under continuous 
                           development by Gage Clawson, Camilla Novaglio & 
                           Julia Blanchard from the Institute for Marine & 
                           Antarctic Studies (IMAS), University of Tasmania. "),
                         
                         
                         h3("Caveats and limitations", style = "margin-top: 30px"),
                         p("This data is likely not comprehensive and represents modeled outputs only. For example, 
                           estimates in Southeast Asia, aside 
                           from China, are likely too concentrated for some years (e.g.,
                           Indonesia). This is an artifact of insufficient AIS 
                           data in this region."),
                         p("Additionally, users should be aware that historical 
                           predictions (1950-2014) may not capture:"),
                         tags$ul(
                           tags$li("Technological changes in fishing capabilities"),
                           tags$li("Evolution of fishing strategies and practices"),
                           tags$li("Changes in management regulations"),
                           tags$li("Shifts in target species or fishing grounds 
                                   due to socio-economic factors")
                         ),
                         
                         
                         h3("Rousseau et al. 2024 Data Tab", style = "margin-top: 30px;"),
                         p("The ", strong("'Rousseau et al. 2024 data' tab"), 
                           " provides access to an independent dataset of mapped 
                           global fishing activity from ", 
                           tags$a(href = "https://www.nature.com/articles/s41597-023-02824-6", 
                                  "Rousseau et al. 2024"), ": \"A database of mapped 
                           global fishing activity 1950–2017\". This dataset 
                           represents a previous spatial allocation of 
                           fishing effort data that is different from the modeled 
                           estimates shown in the other tabs."),
                         
                         p("The Rousseau et al. 2024 dataset offers additional 
                           grouping options including functional groups, allowing for detailed analysis of fishing 
                           patterns by different fleet characteristics. Users 
                           can explore this data by individual flag country, 
                           with the same temporal coverage (1950-2017) and 
                           location filtering options (EEZ and FAO areas) as 
                           the other tabs. Unfortunately, the mapped data (with latitude and longitude points) for the Rousseau data
                           is too large to add to this shiny app, however, this data can be accessed via the IMAS data portal here: 
                           https://data.imas.utas.edu.au/attachments/1241a51d-c8c2-4432-aa68-3d2bae142794/"),
                         
                         h3("How should I use this tool?", style = "margin-top: 30px;"),
                         p("This app has three tabs that allow you to visualise 
                           and download fishing effort data:"),
                         tags$ul(
                           tags$li(strong("The 'Map' tab"), " 
                                   allows you to explore spatially explicit 
                                   industrial effort data globally and for a 
                                   selected region (EEZ or FAO statistical area). 
                                   You can also specify the year (between 1950 
                                   and 2017), flag country (e.g. Angola, Albania
                                   , Argentina), sector (industrial or artisanal), 
                                   gear type (e.g. bottom trawling, longline), 
                                   and vessel length category (less than 6m, 
                                   6-12m, 12-24m, 24-50m, over 50m) you are 
                                   interested in exploring."),
                           tags$li(strong("The 'Time series' tab"), " gives you 
                                   the same options but allows you to explore 
                                   trends in fishing effort across both industrial 
                                   and artisanal sectors combined."),
                           tags$li(strong("The 'Rousseau et al. 2024 data' tab"), " 
                                   provides access to an independent dataset with 
                                   additional grouping options including functional 
                                   groups and sectors, allowing for detailed 
                                   country-specific analysis.")
                         ),
                         
                         h3("How should I cite data from this site?", style = "margin-top: 30px;"),
                         p("You can download the data used to create the plots 
                           shown in this interactive tool using the 'Download' 
                           button included under each tab. Additionally, all 
                           model data is available via zenodo and our GitHub 
                           repository.
                           As a condition of this tool to access data, you must 
                           cite its use: Clawson, S.G., Novaglio, C., & 
                           Blanchard J.L. (2025). Global Fishing Effort Model 
                           Data and Shiny App:", 
                           tags$a(href = "https://zenodo.org/records/15117266",
                                  "10.5281/zenodo.15110744.")),
                         
                         h3("How can I contact you?", style = "margin-top: 30px;"),
                         p("If you have any ideas on how to improve this app or 
                           if you found any issues, you can \"create an issue\" 
                           in our", 
                           tags$a(href = "https://github.com/Global-Fishing-Effort/mapping_fishing_effort_app",
                                  "GitHub repository.")),
                         p("For general enquiry we can contact Julia Blanchard 
                           at ", tags$a("julia.blanchard@utas.edu.au", 
                                        href = "mailto:julia.blanchard@utas.edu.au")),
                         
                         h3("Acknowledgments", style = "margin-top: 30px;"),
                         p("The development of this app was funded by the Food 
                           and Agriculture Organization of the United Nation 
                           (FAO) and the Minderoo Foundation. We would also like to acknowledge the use of 
                           computing facilities provided by Digital Research 
                           Services, IT Services at the University of Tasmania."),
                         br()
                       )
              )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load world map data
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Store previous selections
  previous_location_selections <- reactiveVal(NULL)
  previous_flag_selections <- reactiveVal(NULL)
  
  # Update flag country selection when it changes
  observeEvent(input$flag_country, {
    previous_flag_selections(input$flag_country)
  })
  
  # Reactive function to get the current dataset based on sector selection
  current_data <- reactive({
    if (input$sector == "industrial") {
      return(data_industrial)
    } else {
      return(data_artisanal)
    }
  })
  
  # Reactive function to get the current timeseries dataset based on sector selection
  current_timeseries_data <- reactive({
    if (input$sector == "industrial") {
      return(data_timeseries_ind)
    } else {
      return(data_timeseries_art)
    }
  })
  
  # Reactive function to get the current map dataset based on sector selection
  current_map_data <- reactive({
    if (input$map_sector == "industrial") {
      return(data_industrial)
    } else {
      return(data_artisanal)
    }
  })
  
  # Update UI choices when sector changes (Time Series tab)
  observeEvent(input$sector, {
    current_dataset <- current_data()
    
    # Update flag country choices
    flag_choices <- c("All" = "All", setNames(as.list(sort(unique(current_dataset$flag_country_name))), sort(unique(current_dataset$flag_country_name))))
    updateSelectInput(session, "flag_country", choices = flag_choices, selected = "All")
    
    # Update group variable choices based on sector
    if (input$sector == "industrial") {
      updateRadioButtons(session, "group_var", 
                         choices = c("Gear Type" = "gear", "Vessel Length Category" = "length_category"),
                         selected = "gear")
    } else {
      # For artisanal, only vessel length category is available
      updateRadioButtons(session, "group_var", 
                         choices = c("Vessel Length Category" = "length_category"),
                         selected = "length_category")
    }
  })
  
  # Update UI choices when sector changes (Map tab)
  observeEvent(input$map_sector, {
    current_dataset <- current_map_data()
    
    # Update flag country choices
    flag_choices <- c("All" = "All", setNames(as.list(sort(unique(current_dataset$flag_country_name))), sort(unique(current_dataset$flag_country_name))))
    updateSelectInput(session, "map_flag_country", choices = flag_choices, selected = "All")
    
    # Update group variable choices based on sector
    if (input$map_sector == "industrial") {
      updateRadioButtons(session, "map_group_var", 
                         choices = c("Gear Type" = "gear", "Vessel Length Category" = "length_category"),
                         selected = "gear")
    } else {
      # For artisanal, only vessel length category is available
      updateRadioButtons(session, "map_group_var", 
                         choices = c("Vessel Length Category" = "length_category"),
                         selected = "length_category")
    }
    
    # Update gear choices if gear is selected
    if (input$map_sector == "industrial" && !is.null(input$map_group_var) && input$map_group_var == "gear") {
      gear_choices <- c("All aggregated" = "All_aggregated", "All" = "All", 
                        setNames(as.list(unique(current_dataset$gear)), unique(current_dataset$gear)))
      updateSelectInput(session, "map_gear", choices = gear_choices, selected = "All_aggregated")
    }
    
    # Update length choices
    if (!is.null(input$map_group_var) && input$map_group_var == "length_category") {
      length_choices <- c("All aggregated" = "All_aggregated", "All" = "All", 
                          setNames(as.list(unique(current_dataset$length_category)), unique(current_dataset$length_category)))
      updateSelectInput(session, "map_length", choices = length_choices, selected = "All_aggregated")
    }
  })
  
  # Helper function to get the appropriate column names based on effort type
  get_effort_columns <- function(effort_type) {
    if (effort_type == "nominal") {
      return(list(
        hours = "nom_active_fishing_hours",
        days = "nom_active_fishing_days",
        total_hours = "total_nominal_fishing_hours"
      ))
    } else { # effective
      return(list(
        hours = "eff_active_fishing_hours",
        days = "eff_active_fishing_days",
        total_hours = "total_effective_fishing_hours"
      ))
    }
  }
  
  # Dynamic UI for location selection based on selected flag countries and location type (Time Series tab)
  output$location_selector <- renderUI({
    req(input$flag_country, input$location_type)
    
    # Use the current dataset based on sector selection
    current_dataset <- current_data()
    
    # Get location choices based on flag country selection
    if ("All" %in% input$flag_country) {
      # If "All" flag countries are selected, show all locations
      if (input$location_type == "eez") {
        location_choices <- sort(unique(current_dataset$eez_sovereign_name))
        location_label <- "Select EEZ (Fishing Location):"
        input_id <- "location_selection"
      } else {
        location_choices <- sort(unique(current_dataset$fao_major_fishing_area))
        location_label <- "Select FAO Area (Fishing Location):"
        input_id <- "location_selection"
      }
    } else {
      # Otherwise, filter locations based on selected flag countries
      if (input$location_type == "eez") {
        location_choices <- current_dataset %>%
          filter(flag_country_name %in% input$flag_country) %>%
          pull(eez_sovereign_name) %>%
          unique() %>%
          sort()
        
        location_label <- "Select EEZ (Fishing Location):"
        input_id <- "location_selection"
      } else {
        location_choices <- current_dataset %>%
          filter(flag_country_name %in% input$flag_country) %>%
          pull(fao_major_fishing_area) %>%
          unique() %>% 
          sort()
        
        location_label <- "Select FAO Area (Fishing Location):"
        input_id <- "location_selection"
      }
    }
    
    # Add "All" option at the beginning of the choices
    location_choices_with_all <- c("All" = "All", setNames(as.list(location_choices), location_choices))
    
    # Determine the selected value
    selected_value <- "All"
    if (!is.null(previous_location_selections())) {
      # Check if any of the previous selections are in the current choices
      valid_selections <- intersect(previous_location_selections(), names(location_choices_with_all))
      if (length(valid_selections) > 0) {
        selected_value <- valid_selections
      }
    }
    
    # Create the select input
    tagList(
      selectInput(input_id, location_label,
                  choices = location_choices_with_all,
                  selected = selected_value,
                  multiple = TRUE),
      
      # Add JavaScript to handle the selection logic
      tags$script(HTML("
        $(document).ready(function() {
          // Wait for Shiny to initialize the input
          setTimeout(function() {
            // Get the selectize instance
            var $select = $('#location_selection').selectize();
            var selectize = $select[0].selectize;
            
            // Function to handle selection changes
            selectize.on('item_add', function(value) {
              if (value === 'All') {
                // If 'All' is selected, remove all other selections
                var currentItems = selectize.items.slice();
                currentItems.forEach(function(item) {
                  if (item !== 'All') {
                    selectize.removeItem(item);
                  }
                });
              } else {
                // If any other item is selected, remove 'All'
                if (selectize.items.includes('All')) {
                  selectize.removeItem('All');
                }
              }
            });
          }, 500); // Wait 500ms for Shiny to initialize
        });
      "))
    )
  })
  
  # Dynamic UI for map location selection
  output$map_location_selector <- renderUI({
    req(input$map_flag_country, input$map_location_type)
    
    # Use the current map dataset based on sector selection
    current_dataset <- current_map_data()
    
    # Get location choices based on flag country selection
    if ("All" %in% input$map_flag_country) {
      # If "All" flag countries are selected, show all locations
      if (input$map_location_type == "eez") {
        location_choices <- sort(unique(current_dataset$eez_sovereign_name))
        location_label <- "Select EEZ (Fishing Location):"
        input_id <- "map_location_selection"
      } else {
        location_choices <- sort(unique(current_dataset$fao_major_fishing_area))
        location_label <- "Select FAO Area (Fishing Location):"
        input_id <- "map_location_selection"
      }
    } else {
      # Otherwise, filter locations based on selected flag countries
      if (input$map_location_type == "eez") {
        location_choices <- current_dataset %>%
          filter(flag_country_name %in% input$map_flag_country) %>%
          pull(eez_sovereign_name) %>%
          unique() %>%
          sort()
        
        location_label <- "Select EEZ (Fishing Location):"
        input_id <- "map_location_selection"
      } else {
        location_choices <- current_dataset %>%
          filter(flag_country_name %in% input$map_flag_country) %>%
          pull(fao_major_fishing_area) %>%
          unique() %>% 
          sort()
        
        location_label <- "Select FAO Area (Fishing Location):"
        input_id <- "map_location_selection"
      }
    }
    
    # Add "All" option at the beginning of the choices
    location_choices_with_all <- c("All" = "All", setNames(as.list(location_choices), location_choices))
    
    # Create the select input
    tagList(
      selectInput(input_id, location_label,
                  choices = location_choices_with_all,
                  selected = "All",
                  multiple = TRUE),
      
      # Add JavaScript to handle the selection logic
      tags$script(HTML("
        $(document).ready(function() {
          // Wait for Shiny to initialize the input
          setTimeout(function() {
            // Get the selectize instance
            var $select = $('#map_location_selection').selectize();
            var selectize = $select[0].selectize;
            
            // Function to handle selection changes
            selectize.on('item_add', function(value) {
              if (value === 'All') {
                // If 'All' is selected, remove all other selections
                var currentItems = selectize.items.slice();
                currentItems.forEach(function(item) {
                  if (item !== 'All') {
                    selectize.removeItem(item);
                  }
                });
              } else {
                // If any other item is selected, remove 'All'
                if (selectize.items.includes('All')) {
                  selectize.removeItem('All');
                }
              }
            });
          }, 500); // Wait 500ms for Shiny to initialize
        });
      "))
    )
  })
  
  
  
  # Dynamic UI for gear selector (Map tab)
  output$map_gear_selector <- renderUI({
    # Use default values if inputs are not available yet
    sector <- if(is.null(input$map_sector)) "industrial" else input$map_sector
    group_var <- if(is.null(input$map_group_var)) "gear" else input$map_group_var
    
    if (sector == "industrial" && group_var == "gear") {
      current_dataset <- current_map_data()
      gear_choices <- c("All aggregated" = "All_aggregated", "All" = "All", 
                        setNames(as.list(unique(current_dataset$gear)), unique(current_dataset$gear)))
      selectInput("map_gear", "Select Gear Type:",
                  choices = gear_choices,
                  selected = "All_aggregated")
    } else {
      return(NULL)
    }
  })
  
  # Dynamic UI for length selector (Map tab)
  output$map_length_selector <- renderUI({
    # Use default value if input is not available yet
    group_var <- if(is.null(input$map_group_var)) "gear" else input$map_group_var
    
    if (group_var == "length_category") {
      current_dataset <- current_map_data()
      length_choices <- c("All aggregated" = "All_aggregated", "All" = "All", 
                          setNames(as.list(unique(current_dataset$length_category)), unique(current_dataset$length_category)))
      selectInput("map_length", "Select Vessel Length Category:",
                  choices = length_choices,
                  selected = "All_aggregated")
    } else {
      return(NULL)
    }
  })
  
  # Store the location selections when they change
  observeEvent(input$location_selection, {
    previous_location_selections(input$location_selection)
  })
  
  # Filtered data based on inputs
  filtered_data <- reactive({
    req(input$flag_country, input$location_selection, input$location_type)
    
    # Use the current timeseries dataset based on sector selection
    filtered <- current_timeseries_data()
    
    # Filter by selected flag country if "All" is not selected
    if (!("All" %in% input$flag_country)) {
      filtered <- filtered %>% 
        filter(flag_country_name %in% input$flag_country)
    }
    
    # Check if "All" is selected or not for location
    if (!("All" %in% input$location_selection)) {
      # Filter by selected location (EEZ or FAO area)
      if (input$location_type == "eez") {
        filtered <- filtered %>% 
          filter(eez_sovereign_name %in% input$location_selection)
      } else {
        filtered <- filtered %>% 
          filter(fao_major_fishing_area %in% input$location_selection)
      }
    }
    
    return(filtered)
  })
  
  
  
  # Aggregated data for plotting
  aggregated_data <- reactive({
    req(filtered_data(), input$effort_type)
    
    # Get column names based on effort type
    effort_cols <- get_effort_columns(input$effort_type)
    
    # Group by selected variable and year, then summarize
    filtered_data() %>%
      group_by(year, !!sym(input$group_var)) %>%
      summarize(total_effort = sum(!!sym(effort_cols$days), na.rm = TRUE),
                .groups = "drop")
  })
  
  # Define color palette - expanded to handle more categories
  mypal <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
             "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
             "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5",
             "#c49c94", "#f7b6d3", "#c7c7c7", "#dbdb8d", "#9edae5",
             "#393b79", "#5254a3", "#6b6ecf", "#9c9ede", "#637939",
             "#8ca252", "#b5cf6b", "#cedb9c", "#8c6d31", "#bd9e39")
  
  # Time series plot
  output$timeSeries <- renderPlotly({
    req(aggregated_data(), input$effort_type)
    
    # Ensure all year x group combinations are present
    data_complete <- aggregated_data() %>%
      complete(year = full_seq(year, 1), !!sym(input$group_var), fill = list(total_effort = 0))
    
    group_order <- data_complete %>%
      group_by(!!sym(input$group_var)) %>%
      summarise(total = sum(total_effort, na.rm = TRUE)) %>%
      arrange(desc(total)) %>%
      pull(!!sym(input$group_var))
    
    data_complete[[input$group_var]] <- factor(data_complete[[input$group_var]], levels = group_order)
    
    # Get effort type label
    effort_type_label <- if(input$effort_type == "nominal") "Nominal" else "Effective"
    
    # Get the appropriate label for the legend
    legend_label <- if(input$group_var == "gear") "Gear Type" else "Vessel Length"
    
    # Create a more basic ggplot object with geom_area
    p <- ggplot(data_complete, aes(x = year, y = total_effort, fill = !!sym(input$group_var))) +
      geom_area(stat = "identity", alpha = 0.85, position = "stack") +
      scale_x_continuous(
        breaks = unique(data_complete$year),  # Keep all ticks
        labels = ifelse(unique(data_complete$year) %% 2 == 0 | 
                          unique(data_complete$year) %in% c(1950, 2017), 
                        unique(data_complete$year), "")  # Label every 2nd year + 1950 & 2017
      ) + 
      scale_fill_manual(values = mypal) +
      theme_bw() +
      labs(
        title = paste(effort_type_label, "Fishing Effort Over Time - Modeled"),
        y = "kW days",
        fill = legend_label
      ) +
      theme(axis.text.y = element_text(size = 12),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
            axis.title.y = element_text(size = 12),
            axis.title.x = element_blank(),
            legend.position = "bottom",
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12))
    
    # Convert to plotly with custom tooltip
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  
  # Aggregated data for the map
  map_data <- reactive({
    req(filtered_data(), input$map_effort_type)
    
    # Get column names based on effort type
    effort_cols <- get_effort_columns(input$map_effort_type)
    
    # Group by location and selected variable, then summarize
    if (input$location_type == "eez") {
      location_col <- "eez_sovereign_name"
    } else {
      location_col <- "fao_major_fishing_area"
    }
    
    filtered_data() %>%
      group_by(!!sym(location_col), !!sym(input$group_var)) %>%
      summarize(
        total_effort = sum(!!sym(effort_cols$hours), na.rm = TRUE),
        mean_lon = mean(lon, na.rm = TRUE),
        mean_lat = mean(lat, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # Filtered data for map (using map-specific inputs)
  filtered_data_map <- reactive({
    req(input$map_flag_country, input$map_location_selection, input$map_location_type, input$map_year, input$map_group_var)
    
    # Use the current map dataset based on sector selection
    filtered <- current_map_data()
    
    # Filter by selected flag country if "All" is not selected
    if (!("All" %in% input$map_flag_country)) {
      filtered <- filtered %>% 
        filter(flag_country_name %in% input$map_flag_country)
    }
    
    # Check if "All" is selected or not for location
    if (!("All" %in% input$map_location_selection)) {
      # Filter by selected location (EEZ or FAO area)
      if (input$map_location_type == "eez") {
        filtered <- filtered %>% 
          filter(eez_sovereign_name %in% input$map_location_selection)
      } else {
        filtered <- filtered %>% 
          filter(fao_major_fishing_area %in% input$map_location_selection)
      }
    }
    
    # Filter by selected gear type or vessel length category if a specific category is selected
    # Note: For artisanal data, gear filtering is not applicable since artisanal data doesn't have gear column
    if (input$map_sector == "industrial" && input$map_group_var == "gear" && !is.null(input$map_gear) && input$map_gear != "All" && input$map_gear != "All_aggregated") {
      filtered <- filtered %>% 
        filter(gear == input$map_gear)
    } else if (input$map_group_var == "length_category" && !is.null(input$map_length) && input$map_length != "All" && input$map_length != "All_aggregated") {
      filtered <- filtered %>% 
        filter(length_category == input$map_length)
    }
    
    # Filter by selected year for map
    filtered <- filtered %>% 
      filter(year == input$map_year)
    
    return(filtered)
  })
  
  
  # Map plot 
  output$map <- renderPlot({
    req(filtered_data_map(), input$map_effort_type)
    
    # Get column names based on effort type
    effort_cols <- get_effort_columns(input$map_effort_type)
    
    
    # Create a progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Rendering map...", value = 0)
    on.exit(progress$close())
    
    # Try to create map data
    tryCatch({
      
      # Filter out data with missing coordinates
      valid_data <- filtered_data_map() %>%
        filter(!is.na(lon) & !is.na(lat))
      
      # Check if we have valid data
      if (nrow(valid_data) == 0) {
        return(
          ggplot() +
            geom_sf(data = world, fill = "lightgray", color = "white", size = 0.2) +
            annotate("text", x = 0, y = 0, label = "No effort for this query", 
                     size = 6, color = "darkred", fontface = "bold") +
            theme_bw() 
        )
      }
      
      
      # Round coordinates to create grid cells based on sector
      # Artisanal: 0.5 degree resolution, Industrial: 1 degree resolution
      if (input$map_sector == "artisanal") {
        # For artisanal data, use 0.5 degree resolution
        raster_data <- valid_data %>%
          mutate(
            lon_bin = floor(lon * 2) / 2 + 0.25,  # Center of 0.5-degree cell
            lat_bin = floor(lat * 2) / 2 + 0.25   # Center of 0.5-degree cell
          )
      } else {
        # For industrial data, use 1 degree resolution
        raster_data <- valid_data %>%
          mutate(
            lon_bin = floor(lon) + 0.5,  # Center of 1-degree cell
            lat_bin = floor(lat) + 0.5   # Center of 1-degree cell
          )
      }
      
      # Get the grouping variable based on the radio button selection
      group_var <- input$map_group_var
      
      # Check if we need to aggregate all categories or show individual categories
      if ((group_var == "gear" && input$map_gear == "All_aggregated") || 
          (group_var == "length_category" && input$map_length == "All_aggregated")) {
        # For "All_aggregated", aggregate all categories
        raster_data <- raster_data %>%
          group_by(lon_bin, lat_bin) %>%
          summarize(
            total_effort = sum(!!sym(effort_cols$hours), na.rm = TRUE),
            pixel_area_km2 = mean(pixel_area_km2, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(effort_per_km2 = total_effort/pixel_area_km2)
        
      } else if ((group_var == "gear" && input$map_gear != "All" && input$map_gear != "All_aggregated") || 
                 (group_var == "length_category" && input$map_length != "All" && input$map_length != "All_aggregated")) {
        # For specific category (not "All" and not "All_aggregated"), we don't need to group by the variable
        raster_data <- raster_data %>%
          group_by(lon_bin, lat_bin) %>%
          summarize(
            total_effort = sum(!!sym(effort_cols$hours), na.rm = TRUE),
            pixel_area_km2 = mean(pixel_area_km2, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(effort_per_km2 = total_effort/pixel_area_km2)
        
      } else {
        # For "All" (showing faceted panels), group by the selected variable
        raster_data <- raster_data %>%
          group_by(lon_bin, lat_bin, !!sym(group_var)) %>%
          summarize(
            total_effort = sum(!!sym(effort_cols$hours), na.rm = TRUE),
            pixel_area_km2 = mean(pixel_area_km2, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(effort_per_km2 = total_effort/pixel_area_km2)
        
      }
      
      # Check if all effort is zero
      if (nrow(raster_data) == 0 || sum(raster_data$total_effort, na.rm = TRUE) == 0) {
        return(
          ggplot() +
            geom_sf(data = world, fill = "lightgray", color = "white", size = 0.2) +
            annotate("text", x = 0, y = 0, label = "No effort for this query", 
                     size = 6, color = "darkred", fontface = "bold") +
            coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +
            theme_bw() +
            labs(title = "Modelled Fishing Effort") +
            theme(
              axis.title.x = element_blank(),
              axis.title.y = element_blank()
            )
        )
      }
      
      # Create the base map
      p <- ggplot() +
        # Add world map as background
        geom_sf(data = world, fill = "lightgray", color = "white", size = 0.2) +
        # Set coordinate system and limits
        coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +
        # Set theme
        theme_bw()
      
      # Get the appropriate label for the selected variable
      group_var_label <- if(input$map_group_var == "gear") "Gear Type" else "Vessel Length Category"
      
      # If a specific item is selected or "All aggregated" is selected, create a single map
      if ((input$map_group_var == "gear" && (input$map_gear != "All")) || 
          (input$map_group_var == "length_category" && (input$map_length != "All"))) {
        
        # Get the selected item name or use "All aggregated"
        if (input$map_group_var == "gear") {
          selected_item <- if(input$map_gear == "All_aggregated") "All aggregated" else input$map_gear
        } else {
          selected_item <- if(input$map_length == "All_aggregated") "All aggregated" else input$map_length
        }
        
        breaks <- c(0.02, 0.2, 2, 20, 200, 2000)
        labels <- c("<0.02", "0.02-0.2", "0.2-2", "2-20", "20-200", "200-2000", ">2000")
        colors <- c("#F0F0F0", "#EFF3FE", "#CADBEE", "#A8C9E0", "#E8F4A2", "#F1B16D", "#C54B53")
        names(colors) <- labels  # Ensures colors are mapped by name
        
        raster_data$effort_bin <- cut(raster_data$effort_per_km2,
                                      breaks = c(-Inf, breaks, Inf),
                                      labels = labels,
                                      right = TRUE)
        raster_data$effort_bin <- factor(raster_data$effort_bin, levels = labels)
        
        
        if (input$map_sector == "artisanal") {
          # For artisanal data, use 0.5 degree resolution
          w = 0.5
          h = 0.5
 
        } else {
          # For industrial data, use 1 degree resolution
          w = 1
          h = 1
  
        }
        
        p <- p +
          geom_tile(
            data = raster_data,
            aes(x = lon_bin, y = lat_bin, fill = effort_bin),
            width = w,
            height = h,
            alpha = 0.7
          ) +
          scale_fill_manual(
            name = "Fishing Effort (kW hours/km²)",
            values = colors,
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
          labs(title = "Modelled Fishing Effort")
        
      } else {
        
        breaks <- c(0.02, 0.2, 2, 20, 200, 2000)
        labels <- c("<0.02", "0.02-0.2", "0.2-2", "2-20", "20-200", "200-2000", ">2000")
        colors <- c("#FFFFFF", "#EFF3FE", "#CADBEE", "#A8C9E0", "#E8F4A2", "#F1B16D", "#C54B53")
        names(colors) <- labels  # Ensures colors are mapped by name
        
        raster_data$effort_bin <- cut(raster_data$effort_per_km2,
                                      breaks = c(-Inf, breaks, Inf),
                                      labels = labels,
                                      right = TRUE)
        raster_data$effort_bin <- factor(raster_data$effort_bin, levels = labels)
        
        if (input$map_sector == "artisanal") {
          # For artisanal data, use 0.5 degree resolution
          w = 0.5
          h = 0.5
          
        } else {
          # For industrial data, use 1 degree resolution
          w = 1
          h = 1
          
        }
        
        
        p <- p +
          # Add raster cells for fishing effort
          geom_tile(
            data = raster_data,
            aes(x = lon_bin, y = lat_bin, fill = effort_bin),
            width = w,
            height = h,
            alpha = 0.7
          ) +
          #   # Add facet by the selected variable
          facet_wrap(as.formula(paste("~", input$map_group_var)), ncol = 3) +
          scale_fill_manual(name = "Fishing Effort (kW hours/km²)",
                            values = colors,
                            na.value = "transparent",
                            drop = FALSE) +
          #  Move legend title above and adjust legend size
          guides(fill = guide_legend(title.position = "top", 
                                     title.hjust = 0.5, 
                                     nrow = 1, 
                                     label.position = "bottom")) +
          # Set labels
          labs(
            title = "Modelled Fishing Effort"
          )
        
      }
      
      # Add common theme elements
      p <- p + theme(
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
      
      return(p)
    }, error = function(e) {
      # If there's an error, print it and return a simple plot
      print(paste("Error rendering map:", e$message))
      ggplot() +
        geom_sf(data = world, fill = "lightgray", color = "white", size = 0.2) +
        theme_bw() +
        labs(title = "Error rendering map",
             subtitle = "Check console for details")
    })
  })
  
  # Download handler for Time Series tab
  output$downloadData <- downloadHandler(
    filename = function() {
      # Get the flag countries and location type
      flag_countries <- paste(input$flag_country, collapse = "_")
      location_type <- ifelse(input$location_type == "eez", "EEZ", "FAO")
      
      paste("fishing_effort_data_", flag_countries, "_", location_type, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Use the filtered data for Time Series tab
      data_to_download <- filtered_data() %>%
        mutate(data_source = "Clawson et al. in prep")
      
      # For the download, include all variables from the filtered dataset
      # This includes year, flag_country_name, gear, length_category, eez_sovereign_name, fao_major_fishing_area, fishing hours, and data_source
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )
  
  # Download handler for Map tab
  output$downloadMapData <- downloadHandler(
    filename = function() {
      # Get the flag countries, location type, and year
      flag_countries <- paste(input$map_flag_country, collapse = "_")
      location_type <- ifelse(input$map_location_type == "eez", "EEZ", "FAO")
      year <- input$map_year
      
      paste("fishing_effort_data_", flag_countries, "_", location_type, "_", year, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Use the filtered data for Map tab
      data_to_download <- filtered_data_map() %>%
        mutate(data_source = "Clawson et al. in prep")
      
      # For the download, include all variables from the filtered dataset
      # This includes year, flag_country_name, gear, length_category, eez_sovereign_name, fao_major_fishing_area, fishing hours, and data_source
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )
  
  # ===== ROUSSEAU TAB LOGIC =====
  
  # Initialize Rousseau flag country choices on app start
  observe({
    # Get available countries and map to names using external country mapping
    available_countries <- intersect(rousseau_countries, names(country_names))
    country_choices <- setNames(available_countries, country_names[available_countries])
    country_choices <- country_choices[order(names(country_choices))]
    
    # country_choices <- c("All" = "All", country_choices)
    
    updateSelectInput(session, "rousseau_flag_country", 
                      choices = country_choices, 
                      selected = if(length(country_choices) > 0) country_choices[1] else NULL)
  })
  
  # Reactive function to get Rousseau data for selected country
  rousseau_data <- reactive({
    req(input$rousseau_flag_country)
    
    # Read data for specific country
    data <- read_rousseau_data(input$rousseau_flag_country)
    if (is.null(data)) {
      return(NULL)
    }
    
    # Filter out rows with zero effort
    data <- data %>%
      filter(nom_active > 0 | eff_active > 0)
    
    return(data)
  })
  
  # Helper function to get Rousseau effort columns
  get_rousseau_effort_columns <- function(effort_type) {
    if (effort_type == "nominal") {
      return("nom_active")
    } else {
      return("eff_active")
    }
  }
  
  # Dynamic UI for Rousseau location selection
  output$rousseau_location_selector <- renderUI({
    req(input$rousseau_flag_country, input$rousseau_location_type, rousseau_data())
    
    data <- rousseau_data()
    if (is.null(data)) {
      return(NULL)
    }
    
    # Get location choices based on location type
    if (input$rousseau_location_type == "eez") {
      location_choices <- sort(unique(data$eez_sovereign_name))
      location_label <- "Select EEZ (Fishing Location):"
    } else {
      location_choices <- sort(unique(data$fao_major_fishing_area))
      location_label <- "Select FAO Area (Fishing Location):"
    }
    
    # Add "All" option at the beginning
    location_choices_with_all <- c("All" = "All", setNames(as.list(location_choices), location_choices))
    
    selectInput("rousseau_location_selection", location_label,
                choices = location_choices_with_all,
                selected = "All",
                multiple = TRUE)
  })
  
  # Filtered Rousseau data based on inputs
  filtered_rousseau_data <- reactive({
    req(input$rousseau_flag_country, input$rousseau_location_selection, input$rousseau_location_type)
    
    data <- rousseau_data()
    if (is.null(data)) {
      return(NULL)
    }
    
    # Filter by location if not "All"
    if (!("All" %in% input$rousseau_location_selection)) {
      if (input$rousseau_location_type == "eez") {
        data <- data %>% 
          filter(eez_sovereign_name %in% input$rousseau_location_selection)
      } else {
        data <- data %>% 
          filter(fao_major_fishing_area %in% input$rousseau_location_selection)
      }
    }
    
    return(data)
  })
  
  # Aggregated Rousseau data for plotting
  aggregated_rousseau_data <- reactive({
    req(filtered_rousseau_data(), input$rousseau_effort_type, input$rousseau_group_var)
    
    data <- filtered_rousseau_data()
    if (is.null(data)) {
      return(NULL)
    }
    
    # Get effort column
    effort_col <- get_rousseau_effort_columns(input$rousseau_effort_type)
    
    # Group by selected variable and year, then summarize
    data %>%
      group_by(year, !!sym(input$rousseau_group_var)) %>%
      summarize(total_effort = sum(!!sym(effort_col), na.rm = TRUE),
                .groups = "drop")
  })
  
  # Rousseau time series plot
  output$rousseauTimeSeries <- renderPlotly({
    req(aggregated_rousseau_data(), input$rousseau_effort_type, input$rousseau_group_var)
    
    data_complete <- aggregated_rousseau_data()
    if (is.null(data_complete) || nrow(data_complete) == 0) {
      return(plotly_empty())
    }
    
    # Ensure all year x group combinations are present
    data_complete <- data_complete %>%
      complete(year = full_seq(year, 1), !!sym(input$rousseau_group_var), fill = list(total_effort = 0))
    
    # Order groups by total effort
    group_order <- data_complete %>%
      group_by(!!sym(input$rousseau_group_var)) %>%
      summarise(total = sum(total_effort, na.rm = TRUE)) %>%
      arrange(desc(total)) %>%
      pull(!!sym(input$rousseau_group_var))
    
    data_complete[[input$rousseau_group_var]] <- factor(data_complete[[input$rousseau_group_var]], levels = group_order)
    
    # Get effort type label
    effort_type_label <- if(input$rousseau_effort_type == "nominal") "Nominal" else "Effective"
    
    # Get the appropriate label for the legend
    legend_label <- case_when(
      input$rousseau_group_var == "gear" ~ "Gear Type",
      input$rousseau_group_var == "length_category" ~ "Vessel Length",
      input$rousseau_group_var == "f_group" ~ "Functional Group",
      input$rousseau_group_var == "sector" ~ "Sector",
      TRUE ~ input$rousseau_group_var
    )
    
    # Create ggplot object with geom_area
    p <- ggplot(data_complete, aes(x = year, y = total_effort, fill = !!sym(input$rousseau_group_var))) +
      geom_area(stat = "identity", alpha = 0.85, position = "stack") +
      scale_x_continuous(
        breaks = unique(data_complete$year),
        labels = ifelse(unique(data_complete$year) %% 2 == 0 | 
                          unique(data_complete$year) %in% c(min(data_complete$year), max(data_complete$year)), 
                        unique(data_complete$year), "")
      ) + 
      scale_fill_manual(values = mypal) +
      theme_bw() +
      labs(
        title = paste(effort_type_label, "Fishing Effort Over Time - Rousseau et al. 2024"),
        y = "kW days",
        fill = legend_label
      ) +
      theme(axis.text.y = element_text(size = 12),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
            axis.title.y = element_text(size = 12),
            axis.title.x = element_blank(),
            legend.position = "bottom",
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12))
    
    # Convert to plotly
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # Download handler for Rousseau tab
  output$downloadRousseauData <- downloadHandler(
    filename = function() {
      flag_country <- input$rousseau_flag_country
      location_type <- ifelse(input$rousseau_location_type == "eez", "EEZ", "FAO")
      
      paste("rousseau_fishing_effort_data_", flag_country, "_", location_type, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data_to_download <- filtered_rousseau_data()
      if (!is.null(data_to_download)) {
        data_to_download <- data_to_download %>%
          mutate(data_source = "Rousseau et al. 2024")
        write.csv(data_to_download, file, row.names = FALSE)
      }
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
