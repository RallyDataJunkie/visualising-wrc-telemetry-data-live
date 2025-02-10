library(shiny)
library(dplyr)
library(jsonlite)
library(tidyr)
library(purrr)
library(leaflet)
source("geotools.R")

# Function to extract a specific meta value
extract_meta_value <- function(meta_list, key) {
  value <- meta_list$v[meta_list$n == key]
  if (length(value) == 0) {
    return(NA)
  }
  return(value)
}

SEASON_URL = "https://webappsdata.wrc.com/srv/wrc/json/api/wrcsrv/byType?t=%22Season%22&maxdepth=2"

# Data loading and processing function
load_event_data <- function() {
  # Read JSON data from URL
  json_data <- fromJSON(SEASON_URL)

  # Process the data
  event_data <- json_data %>%
    # Unnest the '_dchildren' column and add a suffix to avoid name conflicts
    unnest(cols = `_dchildren`, names_sep = "_") %>%
    # Extract the meta fields dynamically using the function
    mutate(
      year = name,
      event_name = `_dchildren_name`,
      event_start = map_chr(`_dchildren__meta`, extract_meta_value, key = "date-start"),
      event_finish = map_chr(`_dchildren__meta`, extract_meta_value, key = "date-finish"),
      event_surface = map_chr(`_dchildren__meta`, extract_meta_value, key = "info-surface"),
      kmlmap = map_chr(`_dchildren__meta`, extract_meta_value, key = "kmlfile"),
      time_zone = map_chr(`_dchildren__meta`, extract_meta_value, key = "time-zone"),
      "category" = map_chr(`_dchildren__meta`, extract_meta_value, key = "category"),
      rally_id = map_chr(`_dchildren__meta`, extract_meta_value, key = "Event")
    ) %>%
    select(year, event_name, event_start, event_finish, event_surface, kmlmap, time_zone, category, rally_id)

# TO DO - if we look in _children we get IDs we can look up using 
# https://webappsdata.wrc.com/srv/wrc/json/api/wrcsrv/byId?id=%22{id}%22
# Each record has the stage info
# If we look up the season to depth 3, we can recurse into a _dchildren
# in each event record to get the stages info

  return(event_data)
}

# UI definition
ui <- fluidPage(
  titlePanel("WRC Event Viewer"),
  sidebarLayout(
    sidebarPanel(
      # Year selection
      selectInput("year_select",
        "Select Year:",
        choices = NULL
      ),

      # Category selection - dynamically updated
      selectInput("category_select",
        "Select Category:",
        choices = NULL
      ),

      # Event selection - dynamically updated
      selectInput("event_select",
        "Select Event:",
        choices = NULL
      )
    ),
    mainPanel(
      # Output panel for event details
      wellPanel(
        h3("Event Details"),
        textOutput("event_name"),
        htmlOutput("event_dates"),
        textOutput("event_surface"),
        textOutput("rally_id"),
        leafletOutput("event_map")
      )
    )
  )
)


# Server logic
server <- function(input, output, session) {
  # Load data when app starts
  event_data <- reactive({
    load_event_data()
  })

  # Initialize year dropdown with current year as default
  observe({
    years <- event_data() %>%
      filter(year >= 2023) %>%
      pull(year) %>%
      unique() %>%
      sort()

    current_year <- as.character(format(Sys.Date(), "%Y"))

    # Set default to current year if available, otherwise latest year
    default_year <- if (current_year %in% years) current_year else max(years)

    updateSelectInput(session, "year_select",
      choices = years,
      selected = default_year
    )
  })

  # Update category dropdown based on selected year, default to WRC if available
  observe({
    req(input$year_select)

    categories <- event_data() %>%
      filter(year == input$year_select) %>%
      pull(category) %>%
      unique() %>%
      sort()

    # Set default to "WRC" if available, otherwise first category
    default_category <- if ("WRC" %in% categories) "WRC" else categories[1]

    updateSelectInput(session, "category_select",
      choices = categories,
      selected = default_category
    )
  })

  # Update event dropdown based on selected year and category
  observe({
    req(input$year_select, input$category_select)

    events <- event_data() %>%
      filter(
        year == input$year_select,
        category == input$category_select
      ) %>%
      pull(event_name)

    updateSelectInput(session, "event_select",
      choices = events
    )
  })

  # Display selected event details
  output$event_name <- renderText({
    req(input$event_select)
    paste("Event:", input$event_select)
  })

filtered_event_info <- reactive({
  req(input$event_select, input$year_select)

  event_data() %>%
    filter(
      event_name == input$event_select,
      year == input$year_select
    ) %>%
    head(1)
})

  output$event_dates <- renderUI({
    HTML(paste(
      "<b>Start Date:</b>", filtered_event_info()$event_start, "<br/>",
      "<b>End Date:</b>", filtered_event_info()$event_finish
    ))
  })


  output$event_surface <- renderText({
    paste("Surface:", filtered_event_info()$event_surface)
  })

output$rally_id <- renderText({
  paste("rallyId:", filtered_event_info()$rally_id)
})

  output$event_map <- renderLeaflet({
    # Get the urlstub from the event_info
    urlstub <- filtered_event_info()$kmlmap

    # Get the KML data using the urlstub
    kmlbits <- get_kml_geodf(urlstub)
    kml_df <- kmlbits$kml_df
    stages_kml <- kml_df$geometry

    # Create and return the leaflet map of all stages
    # Use stages_kml[n] for a particular stage
    leaflet(stages_kml) %>%
      addProviderTiles("OpenTopoMap",
        group = "OSM"
      ) %>%
      addPolylines(color = "red", weight = 4)
  })

}

# Run the app
shinyApp(ui = ui, server = server)
