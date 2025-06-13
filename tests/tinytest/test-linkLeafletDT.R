library(tinytest)

# Source the function to test and the helper
source(here::here("R", "link_plots.R"))
source(here::here("tests", "tinytest", "utils", "helper.R"))

test_map_to_table_selection <- function() {
  list(
    test1 = function(session, input, output) {
      session$setInputs(testMap_marker_click = list(id = "WS_Loc_1"))
      expect_equal(input$testTable_rows_selected, 1, 
                   info = "Clicking marker WS_Loc_1 should select row 1")
    },
    test2 = function(session, input, output) {
      session$setInputs(testMap_marker_click = list(id = "WS_Loc_2"))
      expect_equal(input$testTable_rows_selected, 2, 
                   info = "Clicking marker WS_Loc_2 should select row 2")
    }
  )
}

test_table_to_map_selection <- function() {
  list(
    test1 = function(session, input, output) {
      session$setInputs(testTable_rows_selected = 3)
      expect_true(TRUE, info = "Table row selection should not cause errors")
    },
    test2 = function(session, input, output) {
      session$setInputs(testTable_rows_selected = NULL)
      expect_true(TRUE, info = "Deselecting table rows should not cause errors")
    }
  )
}

test_invalid_inputs <- function() {
  list(
    test1 = function(session, input, output) {
      session$setInputs(testMap_marker_click = list(id = "NonExistent_ID"))
      expect_true(
        is.null(input$testTable_rows_selected) || length(input$testTable_rows_selected) == 0,
        info = "Clicking non-existent marker should not select any table row"
      )
    }
  )
}

# Test the linkLeafletDT function using the shared helper
shiny::testServer(
  create_test_server(),
  expr = {
    # Run standard test patterns - pass session, input, output to each test
    map_tests <- test_map_to_table_selection()
    table_tests <- test_table_to_map_selection()
    invalid_tests <- test_invalid_inputs()

    # Execute all tests with proper context
    map_tests$test1(session, input, output)
    map_tests$test2(session, input, output)
    table_tests$test1(session, input, output)
    table_tests$test2(session, input, output)
    invalid_tests$test1(session, input, output)
  }
)

# Test input validation (outside of testServer)
expect_error(linkLeafletDT(), pattern = "missing")
expect_error(
  linkLeafletDT(
    input = list(), session = list(),
    leaflet_output_id = 123, dt_output_id = "test",
    shared_id_column = "id",
    leaflet_data_reactive = reactive(data.frame()),
    dt_data_reactive = reactive(data.frame())
  ),
  pattern = "character strings"
)
