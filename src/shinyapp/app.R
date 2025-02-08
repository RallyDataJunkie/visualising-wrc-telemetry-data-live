library(shiny)
library(dplyr)
library(jsonlite)
library(tidyr)
library(purrr)

# Function to extract a specific meta value
extract_meta_value <- function(meta_list, key) {
  value <- meta_list$v[meta_list$n == key]
  if (length(value) == 0) return(NA)
  return(value)
}


# Data loading and processing function
load_event_data <- function() {
  # Read JSON data from URL
  json_data <- fromJSON("https://webappsdata.wrc.com/srv/wrc/json/api/wrcsrv/byType?t=%22Season%22&maxdepth=2")
  
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
    ) %>%
    select(year, event_name, event_start, event_finish, event_surface, kmlmap, time_zone, category)
  
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
                  choices = NULL),
      
      # Category selection - dynamically updated
      selectInput("category_select", 
                  "Select Category:",
                  choices = NULL),
      
      # Event selection - dynamically updated
      selectInput("event_select", 
                  "Select Event:",
                  choices = NULL)
    ),
    
    mainPanel(
      # Output panel for event details
      wellPanel(
        h3("Event Details"),
        textOutput("event_name"),
        htmlOutput("event_dates"),
        textOutput("event_surface"),
        textOutput("event_map")
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
    years <- sort(unique(event_data()$year))
    current_year <- as.character(format(Sys.Date(), "%Y"))
    
    # Set default to current year if available, otherwise latest year
    default_year <- if(current_year %in% years) current_year else max(years)
    
    updateSelectInput(session, "year_select",
                     choices = years,
                     selected = default_year)
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
    default_category <- if("WRC" %in% categories) "WRC" else categories[1]
    
    updateSelectInput(session, "category_select",
                     choices = categories,
                     selected = default_category)
  })
  
  # Update event dropdown based on selected year and category
  observe({
    req(input$year_select, input$category_select)
    
    events <- event_data() %>%
      filter(year == input$year_select,
             category == input$category_select) %>%
      pull(event_name)
    
    updateSelectInput(session, "event_select",
                     choices = events)
  })
  
  # Display selected event details
  output$event_name <- renderText({
    req(input$event_select)
    paste("Event:", input$event_select)
  })
  
  output$event_dates <- renderUI({
    req(input$event_select, input$year_select)
    
    event_info <- event_data() %>%
      filter(event_name == input$event_select,
             year == input$year_select) %>%  # Added year filter
      head(1)  # Just in case, take only the first match
    
    HTML(paste(
      "<b>Start Date:</b>", event_info$event_start, "<br/>",
      "<b>End Date:</b>", event_info$event_finish
    ))
  })
  
  output$event_surface <- renderText({
    req(input$event_select, input$year_select)
    
    event_info <- event_data() %>%
      filter(event_name == input$event_select,
             year == input$year_select) %>%  # Added year filter
      head(1)  # Just in case, take only the first match
    
    paste("Surface:", event_info$event_surface)
  })
  
  output$event_map <- renderText({
    req(input$event_select, input$year_select)
    
    event_info <- event_data() %>%
      filter(event_name == input$event_select,
             year == input$year_select) %>%  # Added year filter
      head(1)  # Just in case, take only the first match
    
    paste("Map File:", event_info$kmlmap)
  })
}

# Run the app
shinyApp(ui = ui, server = server)