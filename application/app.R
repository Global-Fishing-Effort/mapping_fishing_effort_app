# Loading libraries -------------------------------------------------------
library(arrow)
library(shiny)
library(shinyWidgets)
library(shinycustomloader)
library(bslib)
library(tibble)
library(stringr)
library(dplyr)
library(rnaturalearth)
library(tidyr)
library(sf)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(glue)
library(forcats)
library(qs)
options(scipen = 0)

# Loading supporting files ------------------------------------------------
# Get list of all regions available
effort_data <- qs::qread(file.path("/homevol/fishingeffort/data_storage/prep/random_forest/predictions/model_preds_ZAF.qs")) %>%
  group_by(year, gear, length_category, eez_iso3c) %>%
  summarise(eff_active_fishing_hours = sum(eff_active_fishing_hours, na.rm = TRUE)) %>%
  ungroup()

region_keys <- unique(effort_data$eez_iso3c)

# Define variables for effort and catch datasets we use
effort_variables <- c("gear", "length_category")

data_sets <- c("New methodology", "Rousseau et al.")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


# Defining location of relevant data sources ------------------------------

#Defining palette to be used with effort and catch data
mypal <- c(brewer.pal(n = 9, name = "Set1"), brewer.pal(n = 12, name = "Set3"),
           brewer.pal(n = 8, name = "Accent"))

#Defining plot themes
prettymap_theme <- list(geom_tile(),
                        theme_classic(),
                        scale_fill_viridis_c(na.value = NA),
                        geom_sf(inherit.aes = F, data = world, lwd = 0.25, 
                                color = "black", show.legend = F),
                        theme(text = element_text(colour = "black", size = 15),
                              legend.position = "bottom", 
                              axis.title = element_blank(),
                              legend.key.width = unit(3.5, "cm"),
                              legend.key.height = unit(1, "cm"),
                              plot.title = element_text(size = 18, hjust = 0.5),
                              axis.text.y = element_text(hjust = 0.5, 
                                                         vjust = 0.5, 
                                                         size = 15), 
                              axis.text.x = element_text(angle = 45, 
                                                         hjust = 0.5, 
                                                         vjust = 0.5, 
                                                         size = 15), 
                              legend.ticks = element_line(linewidth = 0.75, 
                                                          colour = "gray"),
                              legend.frame = element_rect(linewidth = 0.75, 
                                                          colour = "dark gray"),
                              legend.text = element_text(size = 15), 
                              legend.title = element_text(size = 15)))

prettyts_theme <- list(theme_bw(),
                       theme(axis.text.y = element_text(size = 14),
                             axis.text.x = element_text(angle = 45, vjust = 1, 
                                                        hjust = 1, size = 14),
                             axis.title.y = element_text(size = 15), 
                             axis.title.x = element_blank(),
                             plot.title = element_text(hjust = 0.5, size = 18),
                             legend.position = "bottom", 
                             legend.text = element_text(size = 18)))


fishing_theme <- list(geom_area(stat = "identity", alpha = 0.85,
                                position = "stack"),
                      scale_x_continuous(breaks = seq(1950, 2017, 5)),
                      scale_fill_manual(values = mypal),
                      theme_bw(),
                      guides(fill = guide_legend(title.position = "top",
                                                 title.hjust = 0.5)),
                      theme(axis.text.y = element_text(size = 12),
                            axis.text.x = element_text(angle = 45, vjust = 1,
                                                       hjust = 1, size = 12),
                            axis.title.y = element_text(size = 12),
                            axis.title.x = element_blank(),
                            legend.position = "bottom",
                            legend.direction = "horizontal",
                            legend.title = element_text(size = 12,
                                                        face = "bold"),
                            legend.text = element_text(size = 12)))



# Defining functions ------------------------------------------------------
# Function to improve map ratios for plotting
scaler <- function(x, type, ratio = F){
  if((x > 0 & type == "min") | (x < 0 & type == "min")){
    x <- ifelse(ratio == T, x-3, x-5)
  }else if((x < 0 & type == "max") | (x > 0 & type == "max")){
    x <- ifelse(ratio == T, x+2, x+4)
  }else if(x == 0 & type == "min"){
    x <- ifelse(ratio == T, x-1, x-2)
  }else{
    x <- ifelse(ratio == T, x+1, x+2)
  }
  return(x)
}

range_map <- function(df, region){
  minx <- min(df$lon)
  maxx <- max(df$lon)
  miny <- min(df$lat)
  maxy <- max(df$lat)
  
  # Calculate range
  rangex <- abs(abs(maxx)-abs(minx))
  rangey <- abs(abs(maxy)-abs(miny))
  
  # Check if map crosses international date line
  if(rangex == 0 & str_detect(region, "southern-ocean", negate = T)){
    df <- df |>
      mutate(lon = lon%%360)
    minx <- min(df$lon)
    maxx <- max(df$lon)
  }
  
  if(rangex >= 1.1*rangey){
    ylims <- c(scaler(miny, "min"),
               scaler(maxy, "max"))
    xlims <- c(scaler(minx, "min", ratio = T),
               scaler(maxx, "max", ratio = T))
  }else if(rangey >= 1.1*rangex){
    xlims <- c(scaler(minx, "min"),
               scaler(maxx, "max"))
    ylims <- c(scaler(miny, "min", ratio = T),
               scaler(maxy, "max", ratio = T))
  }else{
    xlims <- c(scaler(minx, "min"),
               scaler(maxx, "max"))
    ylims <- c(scaler(miny, "min"),
               scaler(maxy, "max"))
  }
  
  return(list(df = df,
              xlims = xlims,
              ylims = ylims))
}


# Defining user interface ------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(bootswatch = "materia"),
  titlePanel(title = span(img( 
    height = 100, width = 300, 
    style = "display: block; margin-left: auto; 
                              margin-right:auto"),
    h1("Global Fishing Effort Explorer",
       style = "color: #095c9e; background-color:#f3f3f3; 
                             border:1.5px solid #c9d5ea; 
                             padding-left: 15px; padding-bottom: 10px; 
                             padding-top: 10px;
                             text-align: center; font-weight: bold")),
    windowTitle = "Global Fishing Effort Explorer"),
  ## Model tab -----------------------------------------------------------------
  tabsetPanel(
    
    ## Catch and effort tab ----------------------------------------------------
    tabPanel("Fishing effort and catch data",
             sidebarLayout(
               sidebarPanel(
                 h4(strong("Instructions:")),
                 
                 # Choose region of interest
                 p("1. Select an EEZ:"),
                 selectInput(inputId = "region_effort", label = NULL,
                             choices = region_keys,
                             selected = "ZAF"),
                 
                 # Choose catch or effort data
                 p("2. Select dataset to visualise:"),
                 radioButtons(inputId = "catch_effort_select", label = NULL, 
                              choiceNames = c("FAO_effort", 
                                              "IMAS_effort"), 
                              choiceValues = c("FAO_effort", "IMAS_effort"),
                              selected = "FAO_effort"),
                 
                 # Choose variable of interest
                 p("3. Select how data should be classified in the plot:"),
                 selectInput(inputId = "variable_effort", 
                             label = NULL,
                             choices = effort_variables,
                             selected = "gear")
               ),
               mainPanel(
                 br(),
                 "The fishing effort and catch data used to create plots in this
                 tab were obtained from XXXX",
                 tags$a(href = "https://github.com/Global-Fishing-Effort/fishing_effort_spatial", 
                        "(Clawson et al. 2025 in prep)."),
                 br(),
                 br(),
                 br()
               )
             )
    )
    
  )
)

# Define actions ---------------------------------------------------------------
server <- function(input, output, session) {
  
  ## Catch and effort tab ------------------------------------------------------
  # Update `variable_effort` choices based on dataset selection; this is because
  # the catch dataset does not have a "Gear" column
  # Loading relevant data based on selection
  selected_data <- reactive({
    if(input$catch_effort_select == "FAO_effort"){
      data <- effort_data
      groups <- effort_variables
    #  y_axis_label <- "Effective fishing effort\n(hours)"
    } else {
      data <- effort_data
      groups <- effort_variables
    }
    
    return(list(data = data,
                groups = groups
              #  y_axis = y_axis_label
                ))
  })
  
  observeEvent(selected_data(), {
    updateSelectInput(session, "variable_effort",
                      choices = selected_data()$groups, 
                      selected = selected_data()$groups[1])
  })
  
  # Filtered data for plotting and downloading
  filtered_data <- reactive({
    req(selected_data()$data)
    df <- selected_data()$data |>
      filter(eez_iso3c == input$region_effort) 
    
    df <- df |>
      group_by(year, !!sym(input$variable_effort)) |>
      summarise(value = sum(eff_active_fishing_hours, na.rm = TRUE)) |>
      ungroup() |>
      mutate(information = glue("<br>Year: {year}<br>{input$variable_effort}: 
                                {get(input$variable_effort)}<br>Value: {value}"))
  })
  
  output$ts_effort <- renderPlotly({
    # Plotting data
    df <- filtered_data()
    
    # Create ggplot
    p <- ggplot(df, aes(x = year, y = value, 
                        fill = !!sym(input$variable_effort), 
                        label = information)) +
      # labs(y = selected_data()$y_axis) +
      fishing_theme
    
    # Convert ggplot to an interactive plot with ggplotly
    ggplotly(p, tooltip = 'label', height = 600, width = 800) |>
      layout(
        legend = list(orientation = "h", xanchor = "center", x = 0.5,
                      yanchor = "top", y = -0.2, font = list(size = 12),
                      traceorder = "normal", title = list(side = "top")),
        margin = list(l = 20, r = 20, t = 10, b = 10)
      )
  })
  
}

shinyApp(ui = ui, server = server)
