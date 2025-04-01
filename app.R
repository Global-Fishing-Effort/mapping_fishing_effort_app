# Fishing Effort Visualization Shiny App
library(shiny)
library(ggplot2)
library(dplyr)
library(qs)
library(plotly)
library(sf)  # For spatial data handling
library(rnaturalearth)  # For world map data
library(htmltools)
library(scales)
library(rnaturalearthdata)
library(shinycssloaders)  # For loading spinners

options(shiny.sanitize.errors = FALSE)

# Read all data files
data_files <- list.files("rf_model_data", pattern = "model_preds_1950_2017_.*\\.qs$", full.names = TRUE)

# Read total effort data for percentage calculations
total_effort_data <- qread("data/total_effort_data.qs")


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

# Read and combine all data files
data <- lapply(data_files, read_data_file) %>%
  bind_rows()

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
                           
                           conditionalPanel(
                             condition = "input.map_group_var === 'gear'",
                             selectInput("map_gear", "Select Gear Type:",
                                         choices = c("All aggregated" = "All_aggregated", 
                                                     "All" = "All", 
                                                     setNames(as.list(unique(data$gear)), unique(data$gear))),
                                         selected = "All_aggregated")
                           ),
                           
                           conditionalPanel(
                             condition = "input.map_group_var === 'length_category'",
                             selectInput("map_length", "Select Vessel Length Category:",
                                         choices = c("All aggregated" = "All_aggregated", 
                                                     "All" = "All", 
                                                     setNames(as.list(unique(data$length_category)), unique(data$length_category))),
                                         selected = "All_aggregated")
                           ),
                           
                           selectInput("map_flag_country", "Select Flag Country (Fishing Fleet):",
                                       choices = c("All" = "All", setNames(as.list(unique(data$flag_country_name)), unique(data$flag_country_name))),
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
                                       min = min(data$year), 
                                       max = max(data$year),
                                       value = max(data$year),
                                       step = 1,
                                       sep = "",
                                       animate = TRUE),
                           
                           # Download button for Map tab
                           downloadButton("downloadMapData", "Download Data")
                         ),
                         
                         mainPanel(
                           conditionalPanel(
                             condition = "input.map_sector === 'industrial'",
                             plotOutput("map", height = "600px"),
                             br(),
                             htmlOutput("mapEffortPercentage")
                           ),
                           conditionalPanel(
                             condition = "input.map_sector === 'artisanal'",
                             br(),
                             h3("We have not modelled artisanal fishing effort for this project", style = "text-align: center; margin-top: 200px;")
                           )
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
                                       choices = c("All" = "All", setNames(as.list(unique(data$flag_country_name)), unique(data$flag_country_name))),
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
                           conditionalPanel(
                             condition = "input.sector === 'industrial'",
                             withSpinner(plotlyOutput("timeSeries", height = 
                                                        "500px"), caption = 
                                           "This takes a few seconds to load"),
                             htmlOutput("timeSeriesEffortPercentage")
                           ),
                           conditionalPanel(
                             condition = "input.sector === 'artisanal'",
                             br(),
                             h3("We have not modelled artisanal fishing effort 
                                for this project", style = "text-align: center; margin-top: 200px;")
                           )
                         )
                       )
              ),
              
              # About Tab (full width)
              tabPanel("About",
                       div(
                         style = "max-width: 1000px; margin: 0 auto; padding: 20px;",
                         h2("About this website", style = "text-align: center; margin-bottom: 20px;"),
                         p("This app provides an interactive platform for
                         exploring and downloading mapped global industrial 
                         fishing effort data. 
                          Users can filter by ", strong("year, country, gear 
                                                        type, vessel length, 
                                                        Exclusive Economic Zone 
                                                        (EEZ), and FAO 
                                                        statistical area"), 
                           " using the selection sidebar in each tab."),
                         
                         p("This latest version of our mapping methodology 
                           integrates country-level fishing effort estimates (", 
                           em(tags$a(href = "https://doi.org/10.1073/pnas.1820344116", "Rousseau et al. 2019")), 
                           ") with a statistical spatial allocation model. 
                            This model is built using AIS-derived fishing activity 
                            from", tags$a(href = "https://globalfishingwatch.org/datasets-and-code/", "Global 
                           Fishing Watch"), "combined with environmental, economic, and governance variables."),
                         
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
                           "using total fishing effort values from ", 
                           em(tags$a(href = "https://doi.org/10.1073/pnas.1820344116", 
                                     "Rousseau et al. 2019")), " (Figure 1)."),
                         # Adding Figure 1 below the text
                         tags$hr(),  # Horizontal line for separation
                         img(src = "flowchart.jpg", height = "auto", width = "100%"),
                         p(strong("Figure 1: "), "Flowchart of methodology,
                           showing data integration (green), statistical
                           modelling approach (blue) and framework to ensure
                           possibilities for future updates (orange).  "),
                         
                         p("Mapped effort estimates are provided as nominal 
                           fishing effort (kilowatt days) or effective fishing 
                           effort (kilowatt days), with a spatial resolution of 
                           1° cell, spanning the years 1950-2017 for 116 
                           countries, covering 90% of the world’s total 
                           industrial fishing effort for 2017. To estimate 
                           effective effort, we have assumed a year-on-year 
                           increase in technical efficiency of 3.5%, as in 
                           Rousseau et al. 2019."),
                         
                         p("This app was created, and is under continuous 
                           development by Gage Clawson, Camilla Novaglio & 
                           Julia Blanchard from the Institute for Marine & 
                           Antarctic Studies (IMAS), University of Tasmania. "),
                         
                         
                         h3("Caveats and limitations", style = "margin-top: 30px"),
                         p("This data is not comprehensive. Currently, the model
                           maps country-level fishing effort for approximately 
                           90% of the country-level global industrial fishing 
                           effort data in 2017 (the most recent year of effort 
                           data). Future iterations of the model are planned to 
                           estimate the remaining 10%, as well as coastal 
                           artisanal effort, that are not well captured by 
                           the AIS dataset. Estimates in Southeast Asia, aside 
                           from China, are likely too concentrated (for example,
                           Indonesia). This is an artifact of insufficient AIS 
                           data in this region. "),
                         p("Additionally, users should be aware that historical 
                           predictions (1950-2014) may not capture:"),
                         tags$ul(
                           tags$li("Technological changes in fishing capabilities"),
                           tags$li("Evolution of fishing strategies and practices"),
                           tags$li("Changes in management regulations"),
                           tags$li("Shifts in target species or fishing grounds 
                                   due to socio-economic factors")
                         ),
                         
                         
                         h3("How should I use this tool?", style = "margin-top: 30px;"),
                         p("This app has two tabs that allow you to visualise 
                           and download fishing effort data:"),
                         tags$ul(
                           tags$li(strong("The 'Map' tab"), " 
                                   allows you to explore spatially explicit 
                                   industrial effort data globally and for a 
                                   selected region (EEZ or FAO statistical area). 
                                   You can also specify the year (between 1950 
                                   and 2017), flag country (e.g. Angola, Albania
                                   , Argentina), gear type (e.g. bottom trawling, longline), 
                                   and vessel length category (less than 6m, 
                                   6-12m, 12-24m, 24-50m, over 50m) you are 
                                   interested in exploring."),
                           tags$li(strong("The 'Time series' tab"), " gives you 
                                   the same options but allows you to explore 
                                   trends in fishing effort.")
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
                           tags$a(href = "https://zenodo.org/records/15110746",
                                  "10.5281/zenodo.15110746.")),
                         
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
                           (FAO). We would also like to acknowledge the use of 
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
    
    # Get location choices based on flag country selection
    if ("All" %in% input$flag_country) {
      # If "All" flag countries are selected, show all locations
      if (input$location_type == "eez") {
        location_choices <- sort(unique(data$eez_sovereign_name))
        location_label <- "Select EEZ (Fishing Location):"
        input_id <- "location_selection"
      } else {
        location_choices <- sort(unique(data$fao_major_fishing_area))
        location_label <- "Select FAO Area (Fishing Location):"
        input_id <- "location_selection"
      }
    } else {
      # Otherwise, filter locations based on selected flag countries
      if (input$location_type == "eez") {
        location_choices <- data %>%
          filter(flag_country_name %in% input$flag_country) %>%
          pull(eez_sovereign_name) %>%
          unique() %>%
          sort()
        
        location_label <- "Select EEZ (Fishing Location):"
        input_id <- "location_selection"
      } else {
        location_choices <- data %>%
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
    
    # Get location choices based on flag country selection
    if ("All" %in% input$map_flag_country) {
      # If "All" flag countries are selected, show all locations
      if (input$map_location_type == "eez") {
        location_choices <- sort(unique(data$eez_sovereign_name))
        location_label <- "Select EEZ (Fishing Location):"
        input_id <- "map_location_selection"
      } else {
        location_choices <- sort(unique(data$fao_major_fishing_area))
        location_label <- "Select FAO Area (Fishing Location):"
        input_id <- "map_location_selection"
      }
    } else {
      # Otherwise, filter locations based on selected flag countries
      if (input$map_location_type == "eez") {
        location_choices <- data %>%
          filter(flag_country_name %in% input$map_flag_country) %>%
          pull(eez_sovereign_name) %>%
          unique() %>%
          sort()
        
        location_label <- "Select EEZ (Fishing Location):"
        input_id <- "map_location_selection"
      } else {
        location_choices <- data %>%
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
  
  
  # Store the location selections when they change
  observeEvent(input$location_selection, {
    previous_location_selections(input$location_selection)
  })
  
  # Filtered data based on inputs
  filtered_data <- reactive({
    req(input$flag_country, input$location_selection, input$location_type)
    
    # Start with all data
    filtered <- data
    
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
    
    # # Filter by year range
    # filtered <- filtered %>% 
    #   filter(year >= input$year_range[1], year <= input$year_range[2])
    
    return(filtered)
  })
  
  # Calculate percentage of total effort for time series
  time_series_percentage <- reactive({
    req(filtered_data(), input$flag_country, input$group_var, input$effort_type)
    
    # Get column names based on effort type
    effort_cols <- get_effort_columns(input$effort_type)
    
    # Get unique years in the filtered data
    years <- unique(filtered_data()$year)
    
    # Calculate percentage for each year
    yearly_percentages <- sapply(years, function(yr) {
      # Get filtered data for this year
      filtered_year <- filtered_data() %>% 
        filter(year == yr)
      
      # Get total fishing hours from filtered data for this year
      filtered_hours <- sum(filtered_year[[effort_cols$hours]], na.rm = TRUE)
      
      # Get total fishing hours from total_effort_data for this year
      total_filtered <- total_effort_data %>%
        filter(year == yr)
      
      # Filter by selected flag country if "All" is not selected
      if (!("All" %in% input$flag_country)) {
        total_filtered <- total_filtered %>% 
          filter(flag_country_name %in% input$flag_country)
      }
      
      # Get total hours for this year
      total_hours <- sum(total_filtered[[effort_cols$total_hours]], na.rm = TRUE)
      
      # Calculate percentage for this year
      if (total_hours > 0) {
        return((filtered_hours / total_hours) * 100)
      } else {
        return(0)
      }
    })
    
    # Calculate average percentage across all years
    avg_percentage <- mean(yearly_percentages, na.rm = TRUE)
    
    return(round(avg_percentage, 1))
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
  
  # Define color palette
  mypal <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
             "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
  
  # Time series plot
  output$timeSeries <- renderPlotly({
    req(aggregated_data(), input$effort_type)
    
    # Get effort type label
    effort_type_label <- if(input$effort_type == "nominal") "Nominal" else "Effective"
    
    # Get the appropriate label for the legend
    legend_label <- if(input$group_var == "gear") "Gear Type" else "Vessel Length"
    
    # Create a more basic ggplot object with geom_area
    p <- ggplot(aggregated_data(), aes(x = year, y = total_effort, fill = !!sym(input$group_var))) +
      geom_area(stat = "identity", alpha = 0.85, position = "stack") +
      # scale_x_continuous(breaks = unique(aggregated_data()$year)) +
      scale_x_continuous(
        breaks = unique(aggregated_data()$year),  # Keep all ticks
        labels = ifelse(unique(aggregated_data()$year) %% 2 == 0 | 
                          unique(aggregated_data()$year) %in% c(1950, 2017), 
                        unique(aggregated_data()$year), "")  # Label every 2nd year + 1950 & 2017
      ) + 
      scale_fill_manual(values = mypal) +
      theme_bw() +
      labs(
        title = paste(effort_type_label, "Fishing Effort Over Time"),
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
        total_effort = sum(!!sym(effort_cols$days), na.rm = TRUE),
        mean_lon = mean(lon, na.rm = TRUE),
        mean_lat = mean(lat, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # Filtered data for map (using map-specific inputs)
  filtered_data_map <- reactive({
    req(input$map_flag_country, input$map_location_selection, input$map_location_type, input$map_year, input$map_group_var)
    
    # Start with all data
    filtered <- data
    
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
    if (input$map_group_var == "gear" && !is.null(input$map_gear) && input$map_gear != "All" && input$map_gear != "All_aggregated") {
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
  
  # Calculate percentage of total effort for map
  map_percentage <- reactive({
    req(filtered_data_map(), input$map_flag_country, input$map_year, input$map_group_var, input$map_effort_type)
    
    # Get column names based on effort type
    effort_cols <- get_effort_columns(input$map_effort_type)
    
    # Get total fishing hours from filtered data
    filtered_hours <- sum(filtered_data_map()[[effort_cols$hours]], na.rm = TRUE)
    
    # Get total fishing hours from total_effort_data based on selected filters
    total_filtered <- total_effort_data %>%
      filter(year == input$map_year)
    
    # Filter by selected flag country if "All" is not selected
    if (!("All" %in% input$map_flag_country)) {
      total_filtered <- total_filtered %>% 
        filter(flag_country_name %in% input$map_flag_country)
    }
    
    # Filter by selected gear type or vessel length category if a specific category is selected
    if (input$map_group_var == "gear" && !is.null(input$map_gear) && input$map_gear != "All" && input$map_gear != "All_aggregated") {
      # Check if the exact gear type exists in the data
      if (input$map_gear %in% unique(total_filtered$gear)) {
        total_filtered <- total_filtered %>% 
          filter(gear == input$map_gear)
      } else {
        # Try a more flexible match - check if the gear type contains the selected gear type
        # This handles cases where UI shows "Seine_Purse_Seine" but data has "Purse_Seine" or similar
        potential_matches <- grep(input$map_gear, unique(total_filtered$gear), value = TRUE)
        
        if (length(potential_matches) > 0) {
          total_filtered <- total_filtered %>% 
            filter(gear %in% potential_matches)
        }
        # If no matches found, keep all rows (don't filter)
      }
    } else if (input$map_group_var == "length_category" && !is.null(input$map_length) && input$map_length != "All" && input$map_length != "All_aggregated") {
      total_filtered <- total_filtered %>% 
        filter(length_category == input$map_length)
    }
    
    # Get total hours
    total_hours <- sum(total_filtered[[effort_cols$total_hours]], na.rm = TRUE)
    
    # Calculate percentage
    if (total_hours > 0) {
      percentage <- (filtered_hours / total_hours) * 100
    } else {
      percentage <- 0
    }
    
    return(round(percentage, 1))
  })
  
  # HTML output for time series effort percentage
  output$timeSeriesEffortPercentage <- renderUI({
    req(time_series_percentage(), input$flag_country, input$location_selection, input$sector, input$effort_type)
    
    # If a specific EEZ is selected (not "All"), don't display percentage information
    if (!("All" %in% input$location_selection)) {
      return(NULL)
    }
    
    
    
    # Format flag country for display
    flag_country_text <- if("All" %in% input$flag_country) "All countries" else paste(input$flag_country, collapse = ", ")
    
    # Format sector for display
    sector_text <- if(input$sector == "industrial") "Industrial fishing" else "Artisanal fishing"
    
    # Format effort type for display
    effort_type_text <- if(input$effort_type == "nominal") "Nominal" else "Effective"
    
    
    # If a specific flag is selected (not "All"), don't say we will gapfill
    if (!("All" %in% input$flag_country)) {
      HTML(paste0("<p style='text-align: center; font-style: italic; color: #666;'>",
                  "Displaying approximately <b>", time_series_percentage(), "%</b> of known total ", tolower(effort_type_text), " fishing effort (average across years) for the selection: ", 
                  flag_country_text, ", ", sector_text, 
                  "</p>"))
    }else{
      
      HTML(paste0("<p style='text-align: center; font-style: italic; color: #666;'>",
                  "Displaying approximately <b>", time_series_percentage(), "%</b> of known total ", tolower(effort_type_text), " fishing effort (average across years) for the selection: ", 
                  flag_country_text, ", ", sector_text, ".<br>Any remaining missing percentage will be estimated in future iterations of the model.",
                  "</p>"))
      
    }
  })
  
  # HTML output for map effort percentage
  output$mapEffortPercentage <- renderUI({
    req(input$map_flag_country, input$map_year, input$map_group_var, input$map_location_selection, input$map_sector, input$map_effort_type)
    
    # If a specific EEZ is selected (not "All"), don't display percentage information
    if (!("All" %in% input$map_location_selection)) {
      return(NULL)
    }
    
    # Format flag country for display
    flag_country_text <- if("All" %in% input$map_flag_country) "All countries" else paste(input$map_flag_country, collapse = ", ")
    
    # Format sector for display
    sector_text <- if(input$map_sector == "industrial") "Industrial fishing" else "Artisanal fishing"
    
    # Format effort type for display
    effort_type_text <- if(input$map_effort_type == "nominal") "Nominal"
    
    # Get gear type or vessel length category text
    category_text <- ""
    if(input$map_group_var == "gear") {
      if(input$map_gear == "All_aggregated") {
        category_text <- "All gear types (aggregated)"
      } else if(input$map_gear == "All") {
        category_text <- "All gear types"
      } else {
        category_text <- input$map_gear
      }
    } else { # length_category
      if(input$map_length == "All_aggregated") {
        category_text <- "All vessel length categories (aggregated)"
      } else if(input$map_length == "All") {
        category_text <- "All vessel length categories"
      } else {
        category_text <- input$map_length
      }
    }
    
    # Get column names based on effort type
    effort_cols <- get_effort_columns(input$map_effort_type)
    
    # Get filtered data for this selection
    filtered_hours <- sum(filtered_data_map()[[effort_cols$hours]], na.rm = TRUE)
    
    # Get total fishing hours from total_effort_data based on selected filters
    total_filtered <- total_effort_data %>%
      filter(year == input$map_year)
    
    # Filter by selected flag country if "All" is not selected
    if (!("All" %in% input$map_flag_country)) {
      total_filtered <- total_filtered %>% 
        filter(flag_country_name %in% input$map_flag_country)
    }
    
    # Check if "All" is selected for gear type or vessel length category
    if ((input$map_group_var == "gear" && input$map_gear == "All") || 
        (input$map_group_var == "length_category" && input$map_length == "All")) {
      
      # Get the categories with known effort in total_effort_data
      if (input$map_group_var == "gear") {
        known_categories <- total_filtered %>%
          filter(!!sym(effort_cols$total_hours) > 0) %>%
          pull(gear) %>%
          unique()
      } else { # length_category
        known_categories <- total_filtered %>%
          filter(!!sym(effort_cols$total_hours) > 0) %>%
          pull(length_category) %>%
          unique()
      }
      
      # Get the categories with modelled predictions in filtered_data_map
      if (input$map_group_var == "gear") {
        modelled_categories <- filtered_data_map() %>%
          filter(!!sym(effort_cols$hours) > 0) %>%
          pull(gear) %>%
          unique()
      } else { # length_category
        modelled_categories <- filtered_data_map() %>%
          filter(!!sym(effort_cols$hours) > 0) %>%
          pull(length_category) %>%
          unique()
      }
      
      # # Find categories with known effort but no modelled predictions
      # missing_categories <- setdiff(known_categories, modelled_categories)
      
      # Calculate percentage of known effort that is modelled
      total_hours <- sum(total_filtered[[effort_cols$total_hours]], na.rm = TRUE)
      percentage <- if (filtered_hours > 0 && total_hours > 0) {
        round((filtered_hours / total_hours) * 100, 1)
      } else {
        0
      }
      
      # Create the HTML output
      output_html <- paste0("<p style='text-align: center; font-style: italic; color: #666;'>",
                            "Displaying approximately <b>", percentage, "%</b> of known total fishing effort for the selection: ", 
                            flag_country_text, ", ", sector_text, ", ", category_text, ", ", input$map_year, 
                            ".<br>Any remaining missing percentage will be estimated in future iterations of the model.",
                            "</p>")
      
      return(HTML(output_html))
      
    } else {
      # For specific category or "All_aggregated" selection
      
      # Filter total_filtered by selected gear type or vessel length category
      if (input$map_group_var == "gear" && !is.null(input$map_gear) && input$map_gear != "All" && input$map_gear != "All_aggregated") {
        total_filtered <- total_filtered %>% 
          filter(gear == input$map_gear)
      } else if (input$map_group_var == "length_category" && !is.null(input$map_length) && input$map_length != "All" && input$map_length != "All_aggregated") {
        total_filtered <- total_filtered %>% 
          filter(length_category == input$map_length)
      }
      
      # Get total hours
      total_hours <- sum(total_filtered[[effort_cols$total_hours]], na.rm = TRUE)
      
      # Check if there is known fishing effort for this selection
      if (total_hours == 0 || is.na(total_hours)) {
        # No known fishing effort
        return(HTML(paste0("<p style='text-align: center; font-style: italic; color: #666;'>",
                           "There is no known fishing effort for this category.",
                           "</p>")))
      } else {
        # There is known fishing effort, calculate percentage
        percentage <- if (filtered_hours > 0) {
          round((filtered_hours / total_hours) * 100, 1)
        } else {
          0
        }
        
        # remove "any remaining will be estiamted in future" if it is 100%.
        
        if (!("All" %in% input$map_flag_country)) {
          return(HTML(paste0("<p style='text-align: center; font-style: italic; color: #666;'>",
                             "Displaying approximately <b>", percentage, "%</b> of known total fishing effort for the selection: ", 
                             flag_country_text, ", ", sector_text, ", ", category_text, ", ", input$map_year, 
                             "</p>")))
        }else{
          
          
          return(HTML(paste0("<p style='text-align: center; font-style: italic; color: #666;'>",
                             "Displaying approximately <b>", percentage, "%</b> of known total fishing effort for the selection: ", 
                             flag_country_text, ", ", sector_text, ", ", category_text, ", ", input$map_year, 
                             ".<br>Any remaining missing percentage will be estimated in future iterations of the model.",
                             "</p>")))
          
        }
      }
    }
  })
  
  # Map plot 
  output$map <- renderPlot({
    req(filtered_data_map(), input$map_effort_type)
    
    # Get column names based on effort type
    effort_cols <- get_effort_columns(input$map_effort_type)
    
    # # Create a progress object
    # progress <- shiny::Progress$new()
    # progress$set(message = "Rendering map...", value = 0)
    # on.exit(progress$close())
    
    
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
            theme_bw() 
        )
      }
      
      
      # Round coordinates to create 1x1 degree grid cells
      raster_data <- valid_data %>%
        mutate(
          lon_bin = floor(lon) + 0.5,  # Center of 1-degree cell
          lat_bin = floor(lat) + 0.5   # Center of 1-degree cell
        )
      
      # Get the grouping variable based on the radio button selection
      group_var <- input$map_group_var
      
      # Check if we need to aggregate all categories or show individual categories
      if ((group_var == "gear" && input$map_gear == "All_aggregated") || 
          (group_var == "length_category" && input$map_length == "All_aggregated")) {
        # For "All_aggregated", aggregate all categories
        raster_data <- raster_data %>%
          group_by(lon_bin, lat_bin) %>%
          summarize(
            total_effort = sum(!!sym(effort_cols$days), na.rm = TRUE),
            .groups = "drop"
          )
      } else if ((group_var == "gear" && input$map_gear != "All" && input$map_gear != "All_aggregated") || 
                 (group_var == "length_category" && input$map_length != "All" && input$map_length != "All_aggregated")) {
        # For specific category (not "All" and not "All_aggregated"), we don't need to group by the variable
        raster_data <- raster_data %>%
          group_by(lon_bin, lat_bin) %>%
          summarize(
            total_effort = sum(!!sym(effort_cols$days), na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        # For "All" (showing faceted panels), group by the selected variable
        raster_data <- raster_data %>%
          group_by(lon_bin, lat_bin, !!sym(group_var)) %>%
          summarize(
            total_effort = sum(!!sym(effort_cols$days), na.rm = TRUE),
            .groups = "drop"
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
        
        p <- p +
          # Add raster cells for fishing effort
          geom_tile(data = raster_data, 
                    aes(x = lon_bin, y = lat_bin, fill = total_effort),
                    alpha = 0.7) +
          # Set fill scale (log scale for better visualization) with consistent max value across years
          scale_fill_viridis_c(name = "kW days (Thousands)",
                               trans = "log10",
                               labels = function(x) scales::comma(x/1000),
                               na.value = "transparent",
                               limits = c(0.1, if(input$map_effort_type == "nominal") 1500000000 else 40000000000) # these values are taken scripts/calculate_max_effort.R
          ) +
          # Move legend title above and adjust legend size
          guides(fill = guide_colorbar(title.position = "top", 
                                       title.hjust = 0.5, 
                                       barwidth = 15,  # Increase legend length
                                       barheight = 0.5)) +  # Reduce height for a sleeker look
          # Set labels
          labs(
            title = "Modelled Fishing Effort"
          )
      } else {
        # Otherwise, create a faceted map by the selected variable
        p <- p +
          # Add raster cells for fishing effort
          geom_tile(data = raster_data, 
                    aes(x = lon_bin, y = lat_bin, fill = total_effort),
                    alpha = 0.7) +
          # Add facet by the selected variable
          facet_wrap(as.formula(paste("~", input$map_group_var)), ncol = 3) +
          # Set fill scale (log scale for better visualization) with consistent max value across years
          scale_fill_viridis_c(name = "kW days (Thousands)",
                               trans = "log10",
                               labels = function(x) scales::comma(x/1000),
                               na.value = "transparent",
                               limits = c(0.1, if(input$map_effort_type == "nominal") 1500000000 else 40000000000)
          ) +
          guides(fill = guide_colorbar(title.position = "top", 
                                       title.hjust = 0.5, 
                                       barwidth = 15,  # Increase legend length
                                       barheight = 0.5)) +  # Reduce height for a sleeker look
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
      data_to_download <- filtered_data()
      
      # For the download, include all variables from the filtered dataset
      # This includes year, flag_country_name, gear, length_category, eez_sovereign_name, fao_major_fishing_area, and fishing hours
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )
  
  # Download handler for Map tab
  output$downloadMapData <- downloadHandler(
    filename = function() {
      # Get the flag countries and location type
      flag_countries <- paste(input$map_flag_country, collapse = "_")
      location_type <- ifelse(input$map_location_type == "eez", "EEZ", "FAO")
      
      paste("fishing_effort_data_", flag_countries, "_", location_type, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Use the filtered data for Map tab
      data_to_download <- filtered_data_map()
      
      # For the download, include all variables from the filtered dataset
      # This includes year, flag_country_name, gear, length_category, eez_sovereign_name, fao_major_fishing_area, and fishing hours
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
