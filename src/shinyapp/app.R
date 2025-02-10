library(shiny)
library(dplyr)
library(jsonlite)
library(tidyr)
library(purrr)
library(leaflet)
library(ggplot2)
library(ggrepel)
library(trajr)
library(rLFT)

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
      category = map_chr(`_dchildren__meta`, extract_meta_value, key = "category")
    ) %>%
    select(year, event_id, event_name, event_start, event_finish, event_surface, kmlmap, time_zone, category)

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
        tableOutput("stage_table"),
        leafletOutput("event_map"),
        plotOutput("stage_geo")
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
    # print(head(kml_df))
    kml_df
  })

  stage_map_sf <- reactive({
    req(input$event_select, input$year_select)

    # Get the urlstub from the event_info
    urlstub <- filtered_event_info()$kmlmap

    # Get the KML data using the urlstub
    kmlbits <- get_kml_geodf(urlstub)
    kml_sf <- kmlbits$kml_sf
    # print(head(kml_df))
    kml_sf
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


  output$stage_geo <- renderPlot({
    req(input$stage_select)
    print(head(stages_info()))
    lookup_val <- stages_info() %>%
      filter(`start-time-control` == input$stage_select) %>%
      pull(kmltrack)

    print(paste("lookup_val", lookup_val))

    # stage_route = stage_map_sf() %>% filter(name == lookup_val)
    utm_routes <- get_utm_projection(stage_map_sf())

    print("this is utm_routes")
    print(head(utm_routes))

    utm_stageroute <- utm_routes %>% filter(name == lookup_val)
    # print(stage_map_sf())
    # tmp = %>%
    # filter(name == lookup_val)
    # trj <- TrajFromCoords(as.data.frame(st_coordinates(stage_map_df[1,])))
    # print(trj)
    # %>% unlist()

    trj <- TrajFromCoords(as.data.frame(st_coordinates(utm_stageroute)))
    # print(st_coordinates(utm_routes[1,]))
    # print(stage_map_df()[1,])
    trj$distance <- Mod(trj$displacement)

    # Find the distance of the just completed step
    trj$distance2 <- c(0, TrajStepLengths(trj))

    # Find the distance of the upcoming step
    trj$predist <- c(TrajStepLengths(trj), 0)

    # Find the accumulated distance at each step
    trj$cum_dist <- cumsum(trj$distance)

    # Step angle in radians relative to previous
    trj$stepangle <- c(0, TrajAngles(trj, compass.direction = NULL) * 180 / pi, NA)

    trj$cumstepangle <- cumsum(c(0, TrajAngles(trj, compass.direction = NULL) * 180 / pi, NA))
    trj$stepheading <- c(TrajAngles(trj, compass.direction = 0) * 180 / pi, NA)



    print("this is trj")
    print(head(trj))


    route_convexity <- bct(utm_stageroute,
      # distance between measurements
      step = 10,
      window = 20, ridName = "name"
    )
    # trj

    tight_corners <- route_convexity[abs(route_convexity$ConvexityIndex) > 0.45, ]

    g_curvature <- ggplot() +
      geom_sf(data = utm_stageroute) +
      ggrepel::geom_text_repel(
        data = tight_corners,
        aes(
          label = ConvexityIndex,
          x = Midpoint_X, y = Midpoint_Y,
          color = (ConvexityIndex > 0)
        ),
        size = 2,
        nudge_x = 2000, nudge_y = 500
      ) +
      geom_point(
        data = tight_corners,
        aes(
          x = Midpoint_X, y = Midpoint_Y,
          color = (ConvexityIndex > 0)
        ), shape = 1, size = 1
      ) +
      theme_classic()

    tight_angle <- 45
    g_trjtight <- ggplot(
      data = trj,
      aes(x = x, y = y), size = 0.5
    ) +
      geom_path(color = "grey") +
      geom_point(
        pch = 0, color = "blue",
        data = trj[abs(trj$stepangle) > tight_angle, ]
      ) +
      geom_point(
        data = trj[abs(trj$stepangle) <= tight_angle, ],
        pch = 1, color = "red", size = 0.1
      ) +
      coord_sf() #+
    # geom_point(aes(x=X, y=Y), size=1, col='black',
    #           data=start_location(1))

    # g_curvature
    g_trjtight
  })
}


# Run the app
shinyApp(ui = ui, server = server)
