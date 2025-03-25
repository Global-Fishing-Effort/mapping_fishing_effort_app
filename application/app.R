# Fishing Effort Visualization Shiny App
library(shiny)
library(ggplot2)
library(dplyr)
library(qs)
library(plotly)
library(sf)  # For spatial data handling
library(rnaturalearth)  # For world map data
library(here)
# Removed shinythemes dependency

# Read all data files
data_files <- list.files(here("rf_model_data"), pattern = "model_preds_.*\\.qs$", full.names = TRUE)

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
  # Removed shinythemes dependency
  titlePanel("Global Fishing Effort Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      # Create a conditional panel that changes based on the active tab
      conditionalPanel(
        condition = "input.tabset === 'Time Series'",
        # Controls for Time Series tab
        radioButtons("sector", "Select Sector:",
                     choices = c("Industrial" = "industrial", 
                                 "Artisanal" = "artisanal"),
                     selected = "industrial"),
        
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
        
        sliderInput("year_range", "Year Range:",
                    min = min(data$year), max = max(data$year),
                    value = c(min(data$year), max(data$year)),
                    step = 1, sep = "")
      ),
      
      conditionalPanel(
        condition = "input.tabset === 'Map'",
        # Controls for Map tab
        radioButtons("map_sector", "Select Sector:",
                     choices = c("Industrial" = "industrial", 
                                 "Artisanal" = "artisanal"),
                     selected = "industrial"),
        
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
        
        selectInput("map_year", "Select Year:",
                    choices = sort(unique(data$year)),
                    selected = max(data$year))
        
        #   sliderInput("map_year", "Select Year:",
        #               min = min(data$year), max = max(data$year),
        #               value = c(min(data$year), max(data$year)),
        #               step = 1, sep = "")
      ),
      
      conditionalPanel(
        condition = "input.tabset === 'Data Table'",
        # Controls for Data Table tab
        radioButtons("table_sector", "Select Sector:",
                     choices = c("Industrial" = "industrial", 
                                 "Artisanal" = "artisanal"),
                     selected = "industrial"),
        
        radioButtons("table_group_var", "Group by:",
                     choices = c("Gear Type" = "gear", 
                                 "Vessel Length Category" = "length_category"),
                     selected = "gear"),
        
        selectInput("table_flag_country", "Select Flag Country (Fishing Fleet):",
                    choices = c("All" = "All", setNames(as.list(unique(data$flag_country_name)), unique(data$flag_country_name))),
                    selected = "All",
                    multiple = TRUE),
        
        # Add JavaScript to handle the selection logic for table flag country
        tags$script(HTML("
          $(document).ready(function() {
            // Wait for Shiny to initialize the input
            setTimeout(function() {
              // Get the selectize instance
              var $select = $('#table_flag_country').selectize();
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
        
        radioButtons("table_location_type", "Select Location Type:",
                     choices = c("EEZ" = "eez", 
                                 "FAO Fishing Area" = "fao"),
                     selected = "eez"),
        
        uiOutput("table_location_selector"),
        
        sliderInput("table_year_range", "Year Range:",
                    min = min(data$year), max = max(data$year),
                    value = c(min(data$year), max(data$year)),
                    step = 1, sep = "")
      ),
      
      # Download button (always visible)
      downloadButton("downloadData", "Download Data")
    ),
    
    mainPanel(
      tabsetPanel(id = "tabset",
                  tabPanel("Map",
                           conditionalPanel(
                             condition = "input.map_sector === 'industrial'",
                             plotOutput("map", height = "600px"),
                             br(),
                             p("Map shows fishing effort by location. Color intensity represents fishing effort.")
                           ),
                           conditionalPanel(
                             condition = "input.map_sector === 'artisanal'",
                             br(),
                             h3("We have not modelled artisanal fishing effort for this project", style = "text-align: center; margin-top: 200px;")
                           )),
                  tabPanel("Time Series", 
                           conditionalPanel(
                             condition = "input.sector === 'industrial'",
                             plotlyOutput("timeSeries", height = "500px")
                           ),
                           conditionalPanel(
                             condition = "input.sector === 'artisanal'",
                             br(),
                             h3("We have not modelled artisanal fishing effort for this project", style = "text-align: center; margin-top: 200px;")
                           )),
                  tabPanel("Data Table", 
                           conditionalPanel(
                             condition = "input.table_sector === 'industrial'",
                             br(),
                             dataTableOutput("dataTable")
                           ),
                           conditionalPanel(
                             condition = "input.table_sector === 'artisanal'",
                             br(),
                             h3("We have not modelled artisanal fishing effort for this project", style = "text-align: center; margin-top: 200px;")
                           ))
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
  
  # Dynamic UI for location selection based on selected flag countries and location type (Time Series tab)
  output$location_selector <- renderUI({
    req(input$flag_country, input$location_type)
    
    # Get location choices based on flag country selection
    if ("All" %in% input$flag_country) {
      # If "All" flag countries are selected, show all locations
      if (input$location_type == "eez") {
        location_choices <- sort(unique(data$eez_country_name))
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
          pull(eez_country_name) %>%
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
        location_choices <- sort(unique(data$eez_country_name))
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
          pull(eez_country_name) %>%
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
  
  # Dynamic UI for table location selection
  output$table_location_selector <- renderUI({
    req(input$table_flag_country, input$table_location_type)
    
    # Get location choices based on flag country selection
    if ("All" %in% input$table_flag_country) {
      # If "All" flag countries are selected, show all locations
      if (input$table_location_type == "eez") {
        location_choices <- unique(data$eez_country_name)
        location_label <- "Select EEZ (Fishing Location):"
        input_id <- "table_location_selection"
      } else {
        location_choices <- unique(data$fao_major_fishing_area)
        location_label <- "Select FAO Area (Fishing Location):"
        input_id <- "table_location_selection"
      }
    } else {
      # Otherwise, filter locations based on selected flag countries
      if (input$table_location_type == "eez") {
        location_choices <- data %>%
          filter(flag_country_name %in% input$table_flag_country) %>%
          pull(eez_country_name) %>%
          unique()
        
        location_label <- "Select EEZ (Fishing Location):"
        input_id <- "table_location_selection"
      } else {
        location_choices <- data %>%
          filter(flag_country_name %in% input$table_flag_country) %>%
          pull(fao_major_fishing_area) %>%
          unique()
        
        location_label <- "Select FAO Area (Fishing Location):"
        input_id <- "table_location_selection"
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
            var $select = $('#table_location_selection').selectize();
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
          filter(eez_country_name %in% input$location_selection)
      } else {
        filtered <- filtered %>% 
          filter(fao_major_fishing_area %in% input$location_selection)
      }
    }
    
    # Filter by year range
    filtered <- filtered %>% 
      filter(year >= input$year_range[1], year <= input$year_range[2])
    
    return(filtered)
  })
  
  # Aggregated data for plotting
  aggregated_data <- reactive({
    req(filtered_data())
    
    # Group by selected variable and year, then summarize
    filtered_data() %>%
      group_by(year, !!sym(input$group_var)) %>%
      summarize(total_effort = sum(nom_active_fishing_hours, na.rm = TRUE),
                .groups = "drop")
  })
  
  # Define color palette
  mypal <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
             "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
  
  # Time series plot
  output$timeSeries <- renderPlotly({
    req(aggregated_data())
    
    # Print aggregated data for debugging
    print(head(aggregated_data()))
    
    # Get the appropriate label for the legend
    legend_label <- if(input$group_var == "gear") "Gear Type" else "Vessel Length"
    
    # Create a more basic ggplot object with geom_area
    p <- ggplot(aggregated_data(), aes(x = year, y = total_effort, fill = !!sym(input$group_var))) +
      geom_area(stat = "identity", alpha = 0.85, position = "stack") +
      scale_x_continuous(breaks = unique(aggregated_data()$year)) +
      scale_fill_manual(values = mypal) +
      theme_bw() +
      labs(
        title = "Fishing Effort Over Time",
        y = "Total Nominal Fishing Effort (Hours)",
        fill = legend_label
      ) +
      theme(axis.text.y = element_text(size = 12),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
            axis.title.y = element_text(size = 12),
            axis.title.x = element_blank(),
            legend.position = "bottom",
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12))
    
    # Convert to plotly with custom tooltip
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # Filtered data for data table (using table-specific inputs)
  filtered_data_table <- reactive({
    req(input$table_flag_country, input$table_location_selection, input$table_location_type, input$table_year_range)
    
    # Start with all data
    filtered <- data
    
    # Filter by selected flag country if "All" is not selected
    if (!("All" %in% input$table_flag_country)) {
      filtered <- filtered %>% 
        filter(flag_country_name %in% input$table_flag_country)
    }
    
    # Check if "All" is selected or not for location
    if (!("All" %in% input$table_location_selection)) {
      # Filter by selected location (EEZ or FAO area)
      if (input$table_location_type == "eez") {
        filtered <- filtered %>% 
          filter(eez_country_name %in% input$table_location_selection)
      } else {
        filtered <- filtered %>% 
          filter(fao_major_fishing_area %in% input$table_location_selection)
      }
    }
    
    # Filter by year range
    filtered <- filtered %>% 
      filter(year >= input$table_year_range[1], year <= input$table_year_range[2])
    
    return(filtered)
  })
  
  # Data table
  output$dataTable <- renderDataTable({
    req(filtered_data_table(), input$table_location_type, input$table_group_var)
    
    # Select appropriate location column based on location type
    if (input$table_location_type == "eez") {
      location_col <- "eez_country_name"
    } else {
      location_col <- "fao_major_fishing_area"
    }
    
    # Group by year, gear/length_category, flag_country_name, and location, then sum fishing hours
    filtered_data_table() %>%
      group_by(year, !!sym(input$table_group_var), flag_country_name, !!sym(location_col)) %>%
      summarize(total_fishing_hours = sum(nom_active_fishing_hours, na.rm = TRUE), .groups = "drop") %>%
      arrange(year, !!sym(input$table_group_var), flag_country_name, !!sym(location_col))
  })
  
  # Aggregated data for the map
  map_data <- reactive({
    req(filtered_data())
    
    # Group by location and selected variable, then summarize
    if (input$location_type == "eez") {
      location_col <- "eez_country_name"
    } else {
      location_col <- "fao_major_fishing_area"
    }
    
    filtered_data() %>%
      group_by(!!sym(location_col), !!sym(input$group_var)) %>%
      summarize(
        total_effort = sum(nom_active_fishing_hours, na.rm = TRUE),
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
          filter(eez_country_name %in% input$map_location_selection)
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
  
  # Map plot with progress indicator
  output$map <- renderPlot({
    req(filtered_data_map())
    
    # Create a progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Rendering map...", value = 0)
    on.exit(progress$close())
    
    # Print debugging information
    print("Rendering map...")
    
    # Try to create map data
    tryCatch({
      # Update progress
      progress$set(value = 0.1, detail = "Filtering data...")
      
      # Filter out data with missing coordinates
      valid_data <- filtered_data_map() %>%
        filter(!is.na(lon) & !is.na(lat))
      
      # Check if we have valid data
      if (nrow(valid_data) == 0) {
        return(
          ggplot() +
            geom_sf(data = world, fill = "lightgray", color = "white", size = 0.2) +
            theme_bw() +
            labs(title = "No fishing effort data with valid coordinates for the current selection")
        )
      }
      
      # Update progress
      progress$set(value = 0.3, detail = "Creating raster grid...")
      
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
            total_effort = sum(nom_active_fishing_hours, na.rm = TRUE),
            .groups = "drop"
          )
      } else if ((group_var == "gear" && input$map_gear != "All" && input$map_gear != "All_aggregated") || 
                 (group_var == "length_category" && input$map_length != "All" && input$map_length != "All_aggregated")) {
        # For specific category (not "All" and not "All_aggregated"), we don't need to group by the variable
        raster_data <- raster_data %>%
          group_by(lon_bin, lat_bin) %>%
          summarize(
            total_effort = sum(nom_active_fishing_hours, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        # For "All" (showing faceted panels), group by the selected variable
        raster_data <- raster_data %>%
          group_by(lon_bin, lat_bin, !!sym(group_var)) %>%
          summarize(
            total_effort = sum(nom_active_fishing_hours, na.rm = TRUE),
            .groups = "drop"
          )
      }
      
      print(paste("Number of raster cells:", nrow(raster_data)))
      
      # Update progress
      progress$set(value = 0.7, detail = "Generating plot...")
      
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
          # Set fill scale (log scale for better visualization)
          scale_fill_viridis_c(name = "Fishing Effort (Thousands of Hours)",
                               trans = "log10",
                               labels = function(x) scales::comma(x / 1000),
                               na.value = "transparent") +
          # Set labels
          labs(
            title = "Global Fishing Effort by Location",
            subtitle = paste(group_var_label, ":", selected_item, "- Year:", input$map_year)
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
          # Set fill scale (log scale for better visualization)
          scale_fill_viridis_c(name = "Fishing Effort (Thousands of Hours)",
                               trans = "log10",
                               labels = function(x) scales::comma(x / 1000),
                               na.value = "transparent") +
          # Set labels
          labs(
            title = "Global Fishing Effort by Location",
            subtitle = paste("Showing all", tolower(group_var_label), "categories - Year:", input$map_year)
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
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      # Get the active tab
      active_tab <- input$tabset
      
      # Get the appropriate flag countries and location type based on the active tab
      if (active_tab == "Time Series") {
        flag_countries <- paste(input$flag_country, collapse = "_")
        location_type <- ifelse(input$location_type == "eez", "EEZ", "FAO")
      } else if (active_tab == "Map") {
        flag_countries <- paste(input$map_flag_country, collapse = "_")
        location_type <- ifelse(input$map_location_type == "eez", "EEZ", "FAO")
      } else { # Data Table
        flag_countries <- paste(input$table_flag_country, collapse = "_")
        location_type <- ifelse(input$table_location_type == "eez", "EEZ", "FAO")
      }
      
      paste("fishing_effort_data_", flag_countries, "_", location_type, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Get the active tab
      active_tab <- input$tabset
      
      # Use the appropriate filtered data based on the active tab
      if (active_tab == "Time Series") {
        data_to_download <- filtered_data()
      } else if (active_tab == "Map") {
        data_to_download <- filtered_data_map()
      } else { # Data Table
        data_to_download <- filtered_data_table()
      }
      
      # For the download, include all variables from the filtered dataset
      # This includes year, flag_country_name, gear, length_category, eez_country_name, fao_major_fishing_area, and fishing hours
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
