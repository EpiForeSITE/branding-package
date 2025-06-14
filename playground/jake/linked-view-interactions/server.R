library(shiny)
library(leaflet)
library(DT)
library(dplyr)

server <- function(input, output, session) {

  # list of dummy wastewater sample locations in Utah, where each entry is a vector of latitude and longitude
  UT_city_locations <- list(
    c(40.93, -111.88), # Salt Lake City
    c(41.1, -112.02),  # Ogden
    c(40.79, -111.74), # Provo
    c(37.76, -111.6),  # St. George
    c(40.71, -111.9),  # Logan
    c(40.76, -111.88), # Park City
    c(40.78, -111.89), # Sandy
    c(40.67, -111.89), # Orem
    c(40.76, -111.89), # Draper
    c(40.76, -111.89)  # Lehi
  )

  num_rows <- length(UT_city_locations) # Number of dummy wastewater sample locations

  # Dummy Data for Wastewater Sample Locations
  # In a real app, this would come from a database, CSV, API, etc.
  # Crucially, each location has a unique 'id'.
  dummy_locations <- reactive({
    data.frame(
      id = paste0("WS_Loc_", 1:num_rows), # Unique Location ID
      name = paste("Treatment Plant", LETTERS[1:num_rows]),
      latitude = sapply(UT_city_locations, function(x) x[1]),
      longitude = sapply(UT_city_locations, function(x) x[2]),
      status = sample(c("Normal", "Elevated Risk", "Action Required"), num_rows, replace = TRUE),
      last_sample_value = round(runif(num_rows, 50, 500)),
      stringsAsFactors = FALSE
    )
  })

  # Reactive Value to Store the ID of the Currently Selected Location
  # This will be updated by map clicks or table row selections.
  selected_location_id <- reactiveVal(NULL)

  # Render the Leaflet Map
  output$wastewaterMap <- renderLeaflet({
    locations <- dummy_locations()
    leaflet(data = locations) %>%
      addTiles() %>% # Adds default OpenStreetMap tiles
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~id # IMPORTANT: Assigns the location 'id' to each marker for click events
      )
  })

  # Render the Data Table
  output$wastewaterTable <- renderDT({
    locations <- dummy_locations()
    datatable(
      locations,
      selection = 'single', # Allow only single row selection
      rownames = FALSE,     # Don't show row numbers from R
      options = list(
        searching = FALSE, # Disable global search box for this simple table
        pageLength = 5     # Show 5 rows per page
      )
    )
  })

  # Observe Map Marker Clicks
  observeEvent(input$wastewaterMap_marker_click, {
    clicked_marker_id <- input$wastewaterMap_marker_click$id
    
    # If the clicked marker is already selected, deselect it (toggle)
    if (!is.null(selected_location_id()) && selected_location_id() == clicked_marker_id) {
      selected_location_id(NULL) # Deselect
    } else {
      selected_location_id(clicked_marker_id) # Select the new marker
    }
  })

  # Observe Table Row Selections
  observeEvent(input$wastewaterTable_rows_selected, {
    selected_row_index <- input$wastewaterTable_rows_selected
    locations <- dummy_locations()
    
    if (length(selected_row_index) > 0) { # If a row is actually selected
      # Get the ID of the location from the selected row
      id_from_table <- locations$id[selected_row_index]
      
      # If the table selection matches the current selected ID, do nothing (or deselect if desired)
      # If it's different, update the selected_location_id
      if (is.null(selected_location_id()) || selected_location_id() != id_from_table) {
        selected_location_id(id_from_table)
      }
    }
  })

  # Update Map Based on `selected_location_id`
  # This observer reacts when `selected_location_id()` changes.
  observe({
    current_id <- selected_location_id()
    locations <- dummy_locations()
    map_proxy <- leafletProxy("wastewaterMap", session) # Get a proxy to modify the map

    map_proxy %>% clearPopups() # Clear any existing popups

    if (!is.null(current_id)) {
      selected_data <- locations[locations$id == current_id, ]
      
      if (nrow(selected_data) > 0) {
        # Open a popup on the selected marker
        map_proxy %>% addPopups(
          lng = selected_data$longitude,
          lat = selected_data$latitude,
          popup = paste("<strong>",selected_data$name,"</strong>", "<br>",
                        "ID:", selected_data$id, "<br>",
                        "Status:", selected_data$status, "<br>",
                        "Value:", selected_data$last_sample_value)
        )

        map_proxy %>% flyTo(lng = selected_data$longitude, lat = selected_data$latitude, zoom = 12)
      }
    }
  })

  # Update Table Selection Based on `selected_location_id`
  # This observer reacts when `selected_location_id()` changes.
  observe({
    current_id <- selected_location_id()
    locations <- dummy_locations()
    table_proxy <- dataTableProxy("wastewaterTable", session)

    # Page length defined in renderDT options.
    page_length <- 5 

    if (!is.null(current_id)) {
      selected_row_index <- which(locations$id == current_id) # This is the 1-based index in the full dataset

      if (length(selected_row_index) > 0) {
        # Calculate the target page (1-indexed)
        target_page <- ceiling(selected_row_index / page_length)

        # Get current displayed page (1-indexed) from table state
        # input$wastewaterTable_state$start is 0-indexed start row for the current page
        current_display_page <- 1 # Default if state is not yet available or table is on first page
        if (!is.null(input$wastewaterTable_state$start) && !is.null(input$wastewaterTable_state$length) && input$wastewaterTable_state$length > 0) {
            # Ensure input$wastewaterTable_state$length is the actual page length used by DT
            current_page_length_from_state <- input$wastewaterTable_state$length
            current_display_page <- (input$wastewaterTable_state$start / current_page_length_from_state) + 1
        } else if (!is.null(input$wastewaterTable_state$start) && page_length > 0) {
            # Fallback if state$length isn't available but start is, use configured page_length
            current_display_page <- (input$wastewaterTable_state$start / page_length) + 1
        }


        # Change page if necessary
        # Check if target_page is different from current_display_page
        # Rounding current_display_page as it might be fractional if state updates are slightly off from page_length
        if (round(current_display_page) != target_page) {
          selectPage(table_proxy, target_page)
        }

        # Select the row if it's not already selected or if the selection is different
        if (is.null(input$wastewaterTable_rows_selected) || 
            length(input$wastewaterTable_rows_selected) == 0 ||
            input$wastewaterTable_rows_selected[1] != selected_row_index) {
          selectRows(table_proxy, selected_row_index)
        }
      }
    } else { # current_id is NULL (deselection)
      # If current_id is NULL, clear the table selection
      if (!is.null(input$wastewaterTable_rows_selected) && length(input$wastewaterTable_rows_selected) > 0) {
        selectRows(table_proxy, NULL)
      }
    }
  })

  output$selectedDataPanel <- renderUI({
    current_id <- selected_location_id()
    
    if (!is.null(current_id)) {
      locations <- dummy_locations() # Get the current data
      selected_data <- locations[locations$id == current_id, ]
      
      if (nrow(selected_data) > 0) {
        sample_value <- selected_data$last_sample_value
        location_name <- selected_data$name
        
        # HTML structure for the light blue box
        div(
          style = "background-color: #e0f2f7; padding: 15px; margin-top: 20px; border-radius: 8px; border: 1px solid #b3cde0;",
          tags$h5(style="margin-top:0; color: #005662;", paste("Details for:", location_name)), # Panel title
          tags$hr(style="border-top: 1px solid #b3cde0;"),
          tags$p(
            tags$strong("Sampled Value: "), 
            tags$span(style="font-weight:bold; color: #007bff;", sample_value)
          ),
          tags$p(
            tags$strong("Status: "),
            tags$span(selected_data$status)
          )
          # You can add more details here from selected_data
        )
      } else {
        # This case should ideally not happen if IDs are managed correctly
        div(
          style = "background-color: #f8d7da; color: #721c24; padding: 15px; margin-top: 20px; border-radius: 5px;",
          "Error: Could not find data for the selected ID."
        )
      }
    } else {
      # If nothing is selected, show a placeholder message
      div(
        style = "background-color: #f0f0f0; padding: 15px; margin-top: 20px; border-radius: 5px; text-align: center; color: #6c757d;",
        tags$em("Select a location on the map or table to see details here.")
      )
    }
  })

}
