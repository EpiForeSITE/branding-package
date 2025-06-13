library(shiny)
library(leaflet)
library(DT)
library(dplyr)
library(here)
library(bslib)
source(here::here("R", "link_plots.R"))

# Suppress "no visible global function definition" note for linkLeafletDT
utils::globalVariables("linkLeafletDT")


if (!exists("linkLeafletDT")) {
  warning("linkLeafletDT function not found. Please source it from your utils file (e.g., shiny_link_utils.R). Using a placeholder for now.")
  linkLeafletDT <- function(...) {
    stop("linkLeafletDT function is not defined. Please ensure it is sourced correctly.")
  }
} else {
  print("linkLeafletDT is here!")
}

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  titlePanel("Wastewater Sample Dashboard: Map-Table Interaction"),

  layout_sidebar(
    sidebar = sidebar(
      title = "Controls & Info",
      width = 300, # Adjust sidebar width as needed
      p("This demo shows how clicking on the map can select a row in the table, and selecting a row in the table can highlight a location on the map."),
      p("A shared 'Location ID' is used to link the views.")
    ),
    # Main content area for map and table
    fluidRow(
      column(width = 7, # Column for the map
             h4("Sample Locations Map"),
             leafletOutput("wastewaterMap", height = "550px") # Slightly reduced height to make space
      ),
      column(width = 5, # Column for the data table
             h4("Sample Details Table"),
             DTOutput("wastewaterTable") # DT table output
      )
    )
  ), # End of layout_sidebar

  # New fluidRow for the selected data panel
  fluidRow(
    column(width = 12, # Full width column
           # This uiOutput will be rendered by the server
           uiOutput("selectedDataPanel")
    )
  )
)

server <- function(input, output, session) {
  # List of dummy wastewaterTable_rows_selectedwastewater sample locations in Utah
  UT_city_locations <- list(
    c(40.93, -111.88), # Salt Lake City area
    c(41.1, -112.02), # Ogden area
    c(40.79, -111.74), # Millcreek/Holladay area
    c(37.76, -111.6), # Remote area (original St. George comment likely misplaced)
    c(40.71, -111.9), # West Valley City/Taylorsville area
    c(40.76, -111.88), # Park City area (coords are more SLC downtown)
    c(40.78, -111.89), # Sandy area (coords are more SLC downtown/Avenues)
    c(40.67, -111.89), # Orem area (coords are Murray/Midvale)
    c(40.76, -111.89), # Draper area (coords are SLC downtown)
    c(40.76, -111.89) # Lehi area (coords are SLC downtown)
  )
  num_rows <- length(UT_city_locations)

  # Dummy Data for Wastewater Sample Locations
  dummy_locations <- reactive({
    data.frame(
      id = paste0("WS_Loc_", 1:num_rows),
      name = paste("Treatment Plant", LETTERS[1:num_rows]),
      latitude = sapply(UT_city_locations, function(x) x[1]),
      longitude = sapply(UT_city_locations, function(x) x[2]),
      status = sample(c("Normal", "Elevated Risk", "Action Required"), num_rows, replace = TRUE),
      last_sample_value = round(runif(num_rows, 50, 500)),
      stringsAsFactors = FALSE
    )
  })

  # Render the Leaflet Map
  output$wastewaterMap <- renderLeaflet({
    locations <- dummy_locations()
    leaflet(data = locations) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~id # IMPORTANT: For click events and linking
      )
  })

  # Render the Data Table
  output$wastewaterTable <- renderDT({
    locations <- dummy_locations()
    datatable(
      locations,
      selection = "single",
      rownames = FALSE,
      options = list(
        searching = FALSE,
        pageLength = 5
      )
    )
  })

  # --- Link Leaflet and DT using the helper function ---
  # This replaces several manual observers.
  # Assumes linkLeafletDT is defined (e.g., sourced from shiny_link_utils.R)
  if (exists("linkLeafletDT")) {
    linkLeafletDT(
      input = input,
      session = session,
      leaflet_output_id = "wastewaterMap",
      dt_output_id = "wastewaterTable",
      shared_id_column = "id",
      leaflet_data_reactive = dummy_locations, # Data for map markers
      dt_data_reactive = dummy_locations, # Data for DT table
      map_lng_col = "longitude", # Name of longitude col in leaflet_data_reactive
      map_lat_col = "latitude" # Name of latitude col in leaflet_data_reactive
      # highlight_zoom and highlight_icon can use defaults or be customized here
    )
  } else {
    warning("linkLeafletDT function not defined. Linking will not work.")
  }

  # --- Reactive to get the currently selected location ID ---
  # This ID is derived from the DT table's selection state, which linkLeafletDT keeps in sync.
  reactive_selected_id <- reactive({
    selected_row_indices <- input$wastewaterTable_rows_selected

    if (is.null(selected_row_indices) || length(selected_row_indices) == 0) {
      return(NULL) # No row selected
    }

    # Assuming single selection, take the first selected index
    selected_row_index <- selected_row_indices[1]
    current_dt_data <- dummy_locations()

    # Basic validation for the index
    if (selected_row_index > 0 && selected_row_index <= nrow(current_dt_data)) {
      return(current_dt_data$id[selected_row_index])
    } else {
      # This case should ideally not happen if DT and data are in sync
      warning(paste("Selected row index", selected_row_index, "is out of bounds for DT data."))
      return(NULL)
    }
  })

  # --- Observer to manage custom popups on the map ---
  # This adds/clears your detailed popups based on the selection.
  # linkLeafletDT handles its own highlight marker and simple popup.
  observe({
    current_id <- reactive_selected_id() # Get the ID from our reactive
    locations <- dummy_locations()
    map_proxy <- leafletProxy("wastewaterMap", session)

    map_proxy %>% clearPopups() # Clear any previously added custom popups

    if (!is.null(current_id)) {
      selected_data <- locations[locations$id == current_id, ]

      if (nrow(selected_data) > 0) {
        # Add the detailed popup
        map_proxy %>% addPopups(
          lng = selected_data$longitude,
          lat = selected_data$latitude,
          popup = paste(
            "<strong>", selected_data$name, "</strong>", "<br>",
            "ID:", selected_data$id, "<br>",
            "Status:", selected_data$status, "<br>",
            "Value:", selected_data$last_sample_value
          ),
          layerId = paste0("detailed_popup_", selected_data$id) # Unique ID for this popup layer
        )
      }
    }
  })

  # --- Render the panel showing details of the selected location ---
  output$selectedDataPanel <- renderUI({
    current_id <- reactive_selected_id() # Get the ID from our reactive

    if (!is.null(current_id)) {
      locations <- dummy_locations()
      selected_data <- locations[locations$id == current_id, ]
      
      if (nrow(selected_data) > 0) {
        sample_value <- selected_data$last_sample_value
        location_name <- selected_data$name
        
        div(
          style = "background-color: #e0f2f7; padding: 15px; margin-top: 20px; border-radius: 8px; border: 1px solid #b3cde0;",
          tags$h5(style = "margin-top:0; color: #005662;", paste("Details for:", location_name)),
          # ADD THIS PART TO DISPLAY THE ID:
          tags$p(
            tags$strong("ID: "), 
            tags$span(current_id) 
          ),
          tags$hr(style = "border-top: 1px solid #b3cde0;"),
          tags$p(
            tags$strong("Sampled Value: "),
            tags$span(style = "font-weight:bold; color: #007bff;", sample_value)
          ),
          tags$p(
            tags$strong("Status: "),
            tags$span(selected_data$status)
          )
        )
      } else {
          div(
            style = "background-color: #f0f0f0; padding: 15px; margin-top: 20px; border-radius: 5px; text-align: center; color: #6c757d;",
            tags$em("ERROR")
          )
      }
    } else {
      div(
        style = "background-color: #f0f0f0; padding: 15px; margin-top: 20px; border-radius: 5px; text-align: center; color: #6c757d;",
        tags$em("Select a location on the map or table to see details here.")
      )
    }
  })
}

shinyApp(ui = ui, server = server)
