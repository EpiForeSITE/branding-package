library(shiny)
library(leaflet)
library(DT)

# Mock data that can be reused across tests
create_mock_leaflet_data <- function() {
  data.frame(
    id = c("WS_Loc_1", "WS_Loc_2", "WS_Loc_3"),
    name = c("Treatment Plant A", "Treatment Plant B", "Treatment Plant C"),
    longitude = c(-122.4194, -122.4094, -122.3994),
    latitude = c(37.7749, 37.7849, 37.7949)
  )
}

create_mock_dt_data <- function() {
  data.frame(
    id = c("WS_Loc_1", "WS_Loc_2", "WS_Loc_3"),
    name = c("Treatment Plant A", "Treatment Plant B", "Treatment Plant C"),
    capacity = c(100, 200, 150)
  )
}

# Standard test server factory
create_test_server <- function(additional_setup = NULL) {
  function(input, output, session) {
    # Mock data
    mock_leaflet_data <- create_mock_leaflet_data()
    mock_dt_data <- create_mock_dt_data()
    
    # Create reactive expressions
    leaflet_data_reactive <- reactive({ mock_leaflet_data })
    dt_data_reactive <- reactive({ mock_dt_data })
    
    # Create standard outputs
    output$testMap <- renderLeaflet({
      leaflet(data = leaflet_data_reactive()) %>%
        addTiles() %>%
        addMarkers(
          lng = ~longitude,
          lat = ~latitude,
          layerId = ~id,
          popup = ~name
        )
    })
    
    output$testTable <- DT::renderDT({
      DT::datatable(
        dt_data_reactive(),
        selection = 'single',
        options = list(pageLength = 10)
      )
    })
    
    # Set up the linking using linkLeafletDT
    linkLeafletDT(
      input = input,
      session = session,
      leaflet_output_id = "testMap",
      dt_output_id = "testTable",
      shared_id_column = "id",
      leaflet_data_reactive = leaflet_data_reactive,
      dt_data_reactive = dt_data_reactive,
      map_lng_col = "longitude",
      map_lat_col = "latitude"
    )
    
    # Execute any additional setup provided by specific tests
    if (!is.null(additional_setup)) {
      additional_setup(input, output, session, leaflet_data_reactive, dt_data_reactive)
    }
  }
}
