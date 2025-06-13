library(tinytest)
library(shiny)
library(here)

source(here::here("server.R"))
source_file_path <- file.path("..", "..", "server.R")
if (!file.exists(source_file_path)) {
  source_file_path <- file.path("..", "..", "app.R") 
  if (!file.exists(source_file_path)) {
    stop("Could not find server.R or app.R. Please adjust the path in the test file.")
  }
}

app_path <- file.path("..", "..", "app.R") # Assuming your main app is app.R in the parent dir
if (!file.exists(app_path)) {
  app_path <- file.path("..", "..", "server.R") # Or if you just have server.R to define the server function
  if (!file.exists(app_path)){
    stop("app.R or server.R not found at expected location for testing.")
  }
  source(app_path) # This should make the `server` function available.
} else {
  app_obj <- shinyAppFile(app_path)
  server <- app_obj$serverFunc # Preferred way to get server func
}

# --- Test suite for reactive_selected_id ---
if (exists("server") && is.function(server)) {
  shiny::testServer(
    server, 
    expr = {
      # Test 1: Initially, no row is selected
      session$setInputs(wastewaterTable_rows_selected = NULL)
      panel_output_initial_html <- as.character(output$selectedDataPanel)
      panel_output_initial_string <- paste(panel_output_initial_html, collapse = " ")
      expect_true(
        grepl("Select a location", panel_output_initial_string),
        info = "Panel should show placeholder when no row is selected."
      )
      
      # Test 2: Simulate selecting the first row
      session$setInputs(wastewaterTable_rows_selected = 1)
      panel_output_row1_html <- as.character(output$selectedDataPanel)
      panel_output_row1_string <- paste(panel_output_row1_html, collapse = " ")
      expect_true(
        grepl("Details for: Treatment Plant A", panel_output_row1_string),
        info = "Panel should show details for Plant A when row 1 is selected."
      )
      expect_true(
        grepl("WS_Loc_1", panel_output_row1_string),
        info = "Panel should contain ID WS_Loc_1 for row 1."
      )
      
      # Test 3: Simulate selecting the second row
      session$setInputs(wastewaterTable_rows_selected = 2)
      panel_output_row2_html <- as.character(output$selectedDataPanel)
      panel_output_row2_string <- paste(panel_output_row2_html, collapse = " ")
      expect_true(
        grepl("Details for: Treatment Plant B", panel_output_row2_string),
        info = "Panel should show details for Plant B when row 2 is selected."
      )
      expect_true(
        grepl("WS_Loc_2", panel_output_row2_string),
        info = "Panel should contain ID WS_Loc_2 for row 2."
      )
      
      # Test 4: Simulate deselecting
      session$setInputs(wastewaterTable_rows_selected = NULL)
      panel_output_deselected_html <- as.character(output$selectedDataPanel)
      panel_output_deselected_string <- paste(panel_output_deselected_html, collapse = " ")
      expect_true(
        grepl("Select a location", panel_output_deselected_string),
        info = "Panel should revert to placeholder when selection is cleared."
      )
    }
  )
} else {
  message("Skipping shiny::testServer tests as server function was not found/loaded.")
}
