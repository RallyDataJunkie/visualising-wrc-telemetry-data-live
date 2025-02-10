library(shiny)
library(dplyr)
library(jsonlite)
library(tidyr)
library(purrr)
library(leaflet)
source("geotools.R")

# TO DO
# THis looks interesting: https://shinystoreplus.obi.obianom.com/
# - eg save map data etc to browser storage
# - eg pick up vals from URL so we can share a view for a year/event/stage

# Function to extract a specific meta value
extract_meta_value <- function(meta_list, key) {
  value <- meta_list$v[meta_list$n == key]
  if (length(value) == 0) {
    return(NA)
  }
  return(value)
}

SEASON_URL <- "https://webappsdata.wrc.com/srv/wrc/json/api/wrcsrv/byType?t=%22Season%22&maxdepth=2"

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
      event_id = `_dchildren__id`,
      event_start = map_chr(`_dchildren__meta`, extract_meta_value, key = "date-start"),
      event_finish = map_chr(`_dchildren__meta`, extract_meta_value, key = "date-finish"),
      event_surface = map_chr(`_dchildren__meta`, extract_meta_value, key = "info-surface"),
      kmlmap = map_chr(`_dchildren__meta`, extract_meta_value, key = "kmlfile"),
      time_zone = map_chr(`_dchildren__meta`, extract_meta_value, key = "time-zone"),
      category = map_chr(`_dchildren__meta`, extract_meta_value, key = "category"),
      rally_id = map_chr(`_dchildren__meta`, extract_meta_value, key = "Event")
    ) %>%
    select(year, event_id, event_name, event_start, event_finish, event_surface, kmlmap, time_zone, category, rally_id)

  return(event_data)
}

# Data loading and processing function
load_stages_data <- function(event_id) {
  event_url <- paste0("https://webappsdata.wrc.com/srv/wrc/json/api/wrcsrv/byId?id=%22", event_id, "%22&maxdepth=2")
  # Read JSON data from URL
  json_data <- fromJSON(event_url)

  # TO DO - using the event_id, we can look up eg
  # https://webappsdata.wrc.com/srv/wrc/json/api/wrcsrv/byId?id=%226ad7a7fa-da2c-43bb-93c5-da788d0ab7a5%22&maxdepth=2
  # and use the same pattern as above to get stage info from _dchldren__meta:
  # name, location, distance, date, firstCar, kmltrack
  # print(json_data$`_dchildren`)
  df <- as_tibble(json_data$`_dchildren`)
  df2 <- df %>%
    unnest(`_meta`) %>%
    select(-`_dchildren`) %>%
    pivot_wider(names_from = n, values_from = v, names_repair = "unique")
  # print(head(df2))
  return(df2 %>% select(`start-time-control`, location, distance, date, firstCar, kmlfile, kmltrack))
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
      ),
      # Event selection - dynamically updated
      selectInput("stage_select",
        "Select Stage:",
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
        tableOutput("stage_table"),
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

  # Update stage dropdown based on selected year, category and event
  observe({
    req(input$year_select, input$category_select, input$event_select)

    stages <- load_stages_data(filtered_event_info()$event_id) %>%
      pull(`start-time-control`)

    updateSelectInput(session, "stage_select",
      choices = stages
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

  stages_info <- reactive({
    req(input$event_select, input$year_select)
    load_stages_data(filtered_event_info()$event_id)
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

  output$stage_table <- renderTable({
    stages_info()
  })

  stage_map_df <- reactive({
    req(input$event_select, input$year_select)

    # Get the urlstub from the event_info
    urlstub <- filtered_event_info()$kmlmap

    # Get the KML data using the urlstub
    kmlbits <- get_kml_geodf(urlstub)
    kml_df <- kmlbits$kml_df
    kml_df
  })

  output$event_map <- renderLeaflet({
    kml_df <- stage_map_df()
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
