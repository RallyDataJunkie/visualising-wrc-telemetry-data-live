library(shiny)
library(bslib)
library(dplyr)
library(jsonlite)
library(tidyr)
library(purrr)
library(leaflet)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(trajr)
library(rLFT)
library(sf)
library(xml2)
# library(amt)
source("geotools.R")

# enableBookmarking("url")



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
  # print(event_url)

  # TO DO - using the event_id, we can look up eg
  # https://webappsdata.wrc.com/srv/wrc/json/api/wrcsrv/byId?id=%226ad7a7fa-da2c-43bb-93c5-da788d0ab7a5%22&maxdepth=2
  # and use the same pattern as above to get stage info from _dchldren__meta:
  # name, location, distance, date, firstCar, kmltrack
  # print(json_data$`_dchildren`)
  df <- as_tibble(json_data$`_dchildren`)

  # In advance of a rally, we don't have the dchildren stage info
  # although in principle we can still have a go at stage route analysis
  df2 <- if ("_meta" %in% names(df)) {
    df %>%
      unnest(`_meta`) %>%
      select(-`_dchildren`) %>%
      pivot_wider(names_from = n, values_from = v, names_repair = "unique")
  } else {
    df[0, ]
  }
  # print(head(df2))
  return(df2 %>% select(any_of(c("start-time-control", "location", "distance", "date", "firstCar", "kmlfile", "kmltrack"))))
}

# UI definition
ui <- function(req) {
  page_sidebar(
    theme = bslib::bs_theme(version = 5),
    titlePanel("WRC Stage Viewer"),
    sidebar = sidebar(
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
      # selectInput("stage_select",
      #  "Select Stage Info:",
      #  choices = NULL
      # ),
      selectInput("stage_geo_select",
        "Select Stage Analysis:",
        choices = NULL
      ),
      sliderInput(
        "stage_range",
        "Stage Map Range:",
        min = 0,
        max = 1,
        value = c(0, 1)
      ),
      # bookmarkButton(),
      # textAreaInput("bookmark_url", "Generated URL", "", rows = 2)
    ),
    navset_card_underline(
      # Output panel for event details
      nav_panel(
        "Event Overview",
        textOutput("event_name"),
        htmlOutput("event_dates"),
        textOutput("event_surface"),
        leafletOutput("event_map"),
      ),
      nav_panel("Stage info", tableOutput("stage_table"), ),
      nav_panel(
        "Stage Map",
        leafletOutput("stage_map"),
      ),
      nav_panel(
        "Stage curvature",
        plotOutput("stage_curvature")
      ),
      nav_panel(
        "km sections (route)",
        plotOutput("stage_geo")
      ),
      nav_panel(
        "km sections (curvature)",
        plotOutput("stage_geo_curvature")
      )
    )
  )
}


# Server logic
server <- function(input, output, session) {
  shinyOptions(cache = cachem::cache_disk("./image-cache"))
  # setBookmarkExclude(c("bookmark_url", "event_map_bounds", "event_map_center", "event_map_zoom", "event_map_groups"))
  # observe({
  # Trigger this observer every time an input changes
  #  reactiveValuesToList(input)
  #  session$doBookmark()
  # })

  # Hacky bookmark display; the url is not updated in webR?
  # session$clientData$url_protocol, "//", session$clientData$url_hostname, session$clientData$url_pathname, "?", session$clientData$url_search
  # onBookmarked(function(url) {
  #  updateTextInput(session, "bookmark_url", value = sub("^.*/", "", url))
  # })

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

  # The primary intention is to use the stage data,
  # but this is not available in advance of a rally
  # But we could pull it from the geojson?
  # observe({
  #  req(input$year_select, input$category_select, input$event_select)
  #  stages_df <- load_stages_data(filtered_event_info()$event_id)
  # print(names(stages_df))
  #  stages <- if ("start-time-control" %in% names(stages_df)) {
  #    stages_df %>%
  #      pull("start-time-control")
  #  } else {
  #    character(0)
  #  }
  # print(stages)
  # updateSelectInput(session, "stage_select",
  #   choices = stages
  # )
  # })

  observe({
    req(input$year_select, input$category_select, input$event_select)
    updateSelectInput(session, "stage_geo_select",
      choices = stage_map_sf() %>% pull("name")
    )
  })

  observe({
    req(input$year_select, input$category_select, input$event_select, input$stage_geo_select)
    lookup_val <- input$stage_geo_select
    stage_length <- st_length(stage_map_sf() %>% filter(name == lookup_val))
    stage_length <- as.integer(as.numeric(stage_length))
    # Or round up to next thousand # ceiling(as.numeric(stage_length) / 1000) * 1000
    # print(stage_length)
    updateSliderInput(session, "stage_range",
      min = 0, max = stage_length, value = c(0, stage_length),
      step = 100
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


  output$stage_map <- renderLeaflet({
    req(input$stage_geo_select, input$stage_range)
    lookup_val <- input$stage_geo_select
    # print("CHECK")
    # print(head(stage_map_df()))
    # print(input$stage_select)
    # kml_df <- stage_map_df() %>% filter(name == lookup_val)
    # stages_kml <- kml_df$geometry
    kml_sf <- stage_map_sf() %>% filter(name == lookup_val)
    # Get route length
    # stage_length <- as.integer(as.numeric(st_length(kml_sf)))
    stage_range <- input$stage_range
    start_dist <- stage_range[1] # / stage_length
    end_dist <- stage_range[2] # / stage_length
    # segment <- lwgeom::st_linesubstring(kml_sf$geometry, start_dist, end_dist)
    # segment_sf <- st_sf(geometry = segment)
    # segment_sf = extract_sf_route_segment(kml_sf, start_dist, end_dist)

    segment_sf <- extract_sf_route_segment(kml_sf, start_dist, end_dist)
    # Create and return the leaflet map of all stages
    # Use stages_kml[n] for a particular stage
    # leaflet(stages_kml) %>%
    # leaflet(kml_sf) %>%
    coords <- st_coordinates(segment_sf) # Get coordinates of the line
    start_coord <- coords[1, ] # First point
    end_coord <- coords[nrow(coords), ]
    leaflet(segment_sf) %>%
      addProviderTiles("OpenTopoMap",
        group = "OSM"
      ) %>%
      addPolylines(color = "red", weight = 4) %>%
      addMarkers(lng = start_coord["X"], lat = start_coord["Y"], popup = paste0("Start: ", start_dist, "m into stage.")) %>%
      addMarkers(lng = end_coord["X"], lat = end_coord["Y"], popup = paste0("End: ", end_dist, "m into stage."))
  })

  trj_info <- reactive({
    req(input$stage_geo_select)
    lookup_val <- input$stage_geo_select
    utm_routes <- utm_routes_projection()
    utm_stageroute <- utm_routes %>% filter(name == lookup_val)
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
    trj
  })

  utm_routes_projection <- reactive({
    req(input$stage_geo_select)
    get_utm_projection(stage_map_sf())
  })

  output$stage_curvature2 <- renderPlot({
    # req(input$stage_geo_select)
    lookup_val <- input$stage_geo_select
    utm_routes <- utm_routes_projection()

    utm_stageroute <- utm_routes %>% filter(name == lookup_val)

    route_convexity <- bct(utm_stageroute,
      # distance between measurements
      step = 10,
      window = 20, ridName = "name"
    )

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

    g_curvature
  })

  output$stage_curvature <- renderCachedPlot({
    req(input$stage_geo_select)
    lookup_val <- input$stage_geo_select
    utm_routes <- utm_routes_projection()
    utm_stageroute <- utm_routes %>% filter(name == lookup_val)
    trj <- trj_info()
    tight_angle <- 45
    coords <- st_coordinates(utm_stageroute$geometry)

    # Get the first and last coordinates
    start_point <- coords[1, ] # First coordinate
    end_point <- coords[nrow(coords), ] # Last coordinate

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
      coord_sf() +
      geom_point(aes(x = start_point[1], y = start_point[2]), size = 2, col = "forestgreen") +
      geom_point(aes(x = end_point[1], y = end_point[2]), size = 2, col = "red")
    g_trjtight
  }, cacheKeyExpr = {
    list("stage_curvature", input$stage_geo_select)
  })

  get_stageroute_utm <- reactive({
    req(input$stage_geo_select)
    lookup_val <- input$stage_geo_select
    utm_routes <- utm_routes_projection()
    utm_stageroute <- utm_routes %>% filter(name == lookup_val)
  })

  route_convexity_mapping <- reactive({
    req(input$stage_geo_select)
    utm_stageroute <- get_stageroute_utm()
    bct(utm_stageroute,
      # distance between measurements
      step = 10,
      window = 20, ridName = "name"
    )
  })

  output$stage_geo_curvature <- renderCachedPlot({
    req(input$stage_geo_select)
    # print(head(stages_info()))
    # Lookupval was when we pulled on input$stage_select
    # lookup_val <-  stages_info() %>%
    # filter(`start-time-control` == input$stage_select) %>%
    # pull(kmltrack)
    # print(paste("lookup_val",lookup_val))
    lookup_val <- input$stage_geo_select
    # print("this is stage_map_sf")
    # print(head(stage_map_sf()))

    # stage_route = stage_map_sf() %>% filter(name == lookup_val)
    utm_routes <- utm_routes_projection()

    # print("this is utm_routes")
    # print(head(utm_routes))

    utm_stageroute <- utm_routes %>% filter(name == lookup_val)
    # print(stage_map_sf())
    # tmp = %>%
    # filter(name == lookup_val)
    # trj <- TrajFromCoords(as.data.frame(st_coordinates(stage_map_df[1,])))
    # print(trj)
    # %>% unlist()

    trj <- trj_info()

    # print("this is trj")
    # print(head(trj))
    route_convexity <- route_convexity_mapping()

    # trj

    # g_curvature
    # g_trjtight
    # segment_plot(route_convexity, 0, 1000)

    segment_length <- 1000
    step_length <- 10
    kms <- floor(max(route_convexity$MidMeas) / segment_length)
    # print(kms)

    segment_plots2 <- list()

    # Iterate through each kilometer
    if (kms < 2) {
      segment_plot2(route_convexity, 0, Inf, bar_range = ylim(min(route_convexity$ConvexityIndex), max(route_convexity$ConvexityIndex)))
    } else {
      for (i in 1:(kms)) {
        segment_plots2[[length(segment_plots2) + 1]] <-
          segment_multiplot2(route_convexity, i, step_length, segment_length,
            bar_range = ylim(min(route_convexity$ConvexityIndex), max(route_convexity$ConvexityIndex)), final = (i == kms),
            typ = "convexity"
          )
      }

      ggarrange(
        plotlist = segment_plots2,
        ncol = 5, nrow = ceiling(kms / 4)
      )
    }
  }, cacheKeyExpr = {
    list("stage_geo_curvature", input$stage_geo_select)
  })

  output$stage_geo <- renderCachedPlot({
    req(input$stage_geo_select)
    lookup_val <- input$stage_geo_select
    route_convexity <- route_convexity_mapping()
    segment_length <- 1000
    step_length <- 10
    kms <- floor(max(route_convexity$MidMeas) / segment_length)
    # print(kms)
    # print(head(route_convexity))
    # Create a list to hold each plot as a separate item
    segment_plots <- list()
    if (kms < 2) {
      segment_plot(route_convexity, 0, Inf)
    } else {
      # Iterate through each kilometer
      for (i in 1:kms) {
        # Add each plot to the plot list
        segment_plots[[length(segment_plots) + 1]] <-
          segment_multiplot(route_convexity, i, step_length, segment_length, final = (i == kms))
      }

      g_sections <- ggarrange(
        plotlist = segment_plots,
        ncol = 5, nrow = ceiling(kms / 4)
      )

      annotate_figure(g_sections, left = text_grob("    ", size = 24), top = text_grob(paste0(lookup_val, ": (1 km sections)"), face = "bold", size = 14))
    }
  }, cacheKeyExpr = {
    list("plot_stage_geo", input$stage_geo_select)
  }) # %>% bindCache(input$stage_geo_select)
}


# Run the app
shinyApp(ui = ui, server = server)
