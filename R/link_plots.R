# Helper function to link interactions between a Leaflet map and a DT datatable in a Shiny app.
#
#' Create a two-way link between a Leaflet map and a DT datatable.
#'
#' This function sets up observers so that:
#' 1. Clicking a marker on the Leaflet map will select the corresponding row in the DT table.
#' 2. Selecting a row in the DT table will highlight the corresponding marker on the
#'    Leaflet map and fly the map to that marker.
#'
#' @param input The Shiny `input` object from your server function.
#' @param session The Shiny `session` object from your server function.
#' @param leaflet_output_id A character string: the `outputId` of your `leafletOutput`.
#'   Example: "myMap".
#' @param dt_output_id A character string: the `outputId` of your `DT::DTOutput`.
#'   Example: "myTable".
#' @param shared_id_column A character string: the name of the column that is present
#'   in *both* the Leaflet data and the DT data. This column must contain unique
#'   identifiers for each item. When creating your Leaflet map, ensure you set the
#'   `layerId` for your markers (or other shapes) to the values from this column.
#'   Example: `addMarkers(..., layerId = ~my_unique_id_column)`.
#' @param leaflet_data_reactive A reactive expression that returns the data frame
#'   used to generate the Leaflet map. This data frame MUST contain the
#'   `shared_id_column` and the columns specified by `map_lng_col` and `map_lat_col`.
#'   Example: `reactive({ my_spatial_data_frame })`.
#' @param dt_data_reactive A reactive expression that returns the data frame
#'   displayed in the DT table. This data frame MUST contain the `shared_id_column`.
#'   Example: `reactive({ my_table_data_frame })`.
#' @param map_lng_col (Optional) A character string: the name of the column in
#'   `leaflet_data_reactive` that contains the longitude values for the map markers.
#'   Defaults to "longitude".
#' @param map_lat_col (Optional) A character string: the name of the column in
#'   `leaflet_data_reactive` that contains the latitude values for the map markers.
#'   Defaults to "latitude".
#' @param highlight_zoom (Optional) An integer: the zoom level the map will fly to
#'   when a marker is highlighted. Defaults to 12.
#' @param highlight_icon (Optional) An icon object created by `leaflet::awesomeIcons()`
#'   (or `leaflet::makeAwesomeIcon()`, `leaflet::icons()`) to use for the highlighted
#'   marker on the map. Defaults to a red star icon.
#'
#' @return This function does not return a value. It is called for its side effects,
#'   which are to set up the reactive observers that link the map and table.
#'
#' @section Important Notes for Users:
#' - **`layerId` is Crucial:** For the map-to-table link to work, your Leaflet markers
#'   (or circles, polygons, etc.) *must* have their `layerId` aesthetic mapped to the
#'   `shared_id_column`. For example:
#'   `leaflet() %>% addMarkers(data = my_data, layerId = ~my_id_column, ...)`
#' - **Unique IDs:** The values in your `shared_id_column` should be unique for each
#'   location/item to ensure correct linking.
#' - **Data Reactivity:** Both `leaflet_data_reactive` and `dt_data_reactive` must be
#'   reactive expressions (e.g., created with `reactive({...})`). This allows the
#'   linking to work even if your underlying data changes.
#' - **DT Selection Mode:** For best results, set your DT table to single row selection:
#'   `DT::datatable(..., selection = 'single')`. The function currently focuses on the
#'   first selected row if multiple are somehow selected.
linkLeafletDT <- function(input, session,
                          leaflet_output_id,
                          dt_output_id,
                          shared_id_column,
                          leaflet_data_reactive,
                          dt_data_reactive,
                          map_lng_col = "longitude",
                          map_lat_col = "latitude",
                          highlight_zoom = 12,
                          highlight_icon = leaflet::awesomeIcons(
                            icon = 'star',
                            library = 'glyphicon', # Uses Bootstrap Glyphicons
                            markerColor = 'red',
                            iconColor = '#FFFFFF' # White star on red marker
                          )) {
  
  # --- Input Validation and Checks (Basic) ---
  # Ensure required arguments are provided (shiny::req can be used within observers for reactive inputs)
  if (missing(input) || missing(session) || missing(leaflet_output_id) ||
      missing(dt_output_id) || missing(shared_id_column) ||
      missing(leaflet_data_reactive) || missing(dt_data_reactive)) {
    stop("One or more required arguments for linkLeafletDT are missing.")
  }
  if (!is.character(leaflet_output_id) || !is.character(dt_output_id) ||
      !is.character(shared_id_column) || !is.character(map_lng_col) ||
      !is.character(map_lat_col)) {
    stop("Output IDs, shared_id_column, map_lng_col, and map_lat_col must be character strings.")
  }
  if (!is.reactive(leaflet_data_reactive) || !is.reactive(dt_data_reactive)) {
    stop("leaflet_data_reactive and dt_data_reactive must be reactive expressions.")
  }
  
  
  # --- Observer 1: Leaflet map marker click -> Selects row in DT table ---
  # This observer triggers when a marker on the Leaflet map is clicked.
  # The input name for marker clicks is dynamically generated as `leaflet_output_id + "_marker_click"`.
  observeEvent(input[[paste0(leaflet_output_id, "_marker_click")]], {
    clicked_event <- input[[paste0(leaflet_output_id, "_marker_click")]]
    
    # shiny::req ensures that clicked_event and clicked_event$id are not NULL before proceeding.
    # This prevents errors if the click event data is incomplete.
    shiny::req(clicked_event, !is.null(clicked_event$id))
    
    clicked_marker_id <- clicked_event$id # This ID comes from the `layerId` you set on your markers.
    current_dt_data <- dt_data_reactive() # Get the current data for the DT table.
    
    # Ensure the shared_id_column actually exists in the DT data.
    shiny::req(shared_id_column %in% names(current_dt_data))
    
    # Find the row index in the DT data that matches the clicked marker's ID.
    # `which()` returns a vector of indices; we take the first one if multiple (IDs should be unique).
    row_idx <- which(current_dt_data[[shared_id_column]] == clicked_marker_id)
    
    if (length(row_idx) > 0) {
      # If a matching row is found, select it in the DT table.
      # DT::dataTableProxy allows modifying an existing DT table without redrawing it completely.
      DT::selectRows(DT::dataTableProxy(dt_output_id, session = session), selected = row_idx[1])
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE) # ignoreNULL: Don't run if click is NULL. ignoreInit: Don't run on app start.
  
  
  # --- Observer 2: DT table row selection -> Highlights marker on Leaflet map ---
  # This observer triggers when a row in the DT table is selected (or deselected).
  # The input name for row selections is `dt_output_id + "_rows_selected"`.
  
  # Define a unique group name for the highlight markers. This allows us to easily
  # clear only these specific markers later without affecting other map elements.
  highlight_group_name <- paste0("highlighted_marker_group_", leaflet_output_id, "_", dt_output_id)
  
  observeEvent(input[[paste0(dt_output_id, "_rows_selected")]], {
    selected_row_indices <- input[[paste0(dt_output_id, "_rows_selected")]] # Indices of selected rows.
    
    current_leaflet_data <- leaflet_data_reactive() # Get current data for the Leaflet map.
    current_dt_data <- dt_data_reactive()       # Get current data for the DT table.
    
    # Ensure all necessary columns exist in the reactive data frames.
    shiny::req(
      shared_id_column %in% names(current_dt_data),
      shared_id_column %in% names(current_leaflet_data),
      map_lng_col %in% names(current_leaflet_data),
      map_lat_col %in% names(current_leaflet_data)
    )
    
    # Get a proxy to modify the Leaflet map without full redraw.
    map_proxy <- leaflet::leafletProxy(leaflet_output_id, session = session)
    
    # Always clear any previously added highlight markers from our specific group.
    # This handles deselection or changing selection.
    map_proxy %>% leaflet::clearGroup(group = highlight_group_name)
    
    if (length(selected_row_indices) > 0) {
      # If at least one row is selected (we'll focus on the first one for single selection).
      selected_id_from_table <- current_dt_data[[shared_id_column]][selected_row_indices[1]]
      
      # Find the corresponding point in the Leaflet map data.
      point_to_highlight <- current_leaflet_data[current_leaflet_data[[shared_id_column]] == selected_id_from_table, ]
      
      if (nrow(point_to_highlight) > 0) {
        # If a matching point is found in the map data.
        # Take the first match if IDs happen to not be unique (though they should be).
        point_to_highlight <- point_to_highlight[1, ]
        
        # Add a new, visually distinct marker to the map for the selected point.
        map_proxy %>%
          leaflet::addAwesomeMarkers(
            data = point_to_highlight, # Use the filtered data for this specific point.
            lng = ~get(map_lng_col),    # Access longitude using get() for string column name.
            lat = ~get(map_lat_col),    # Access latitude using get() for string column name.
            # Create a unique layerId for this highlight marker to avoid conflicts.
            layerId = paste0("highlight_", point_to_highlight[[shared_id_column]]),
            icon = highlight_icon,      # Use the specified (or default) highlight icon.
            group = highlight_group_name, # Assign to our specific highlight group.
            popup = ~paste0("Selected: ", htmltools::htmlEscape(as.character(get(shared_id_column)))) # Basic popup for highlight.
          ) %>%
          leaflet::flyTo( # Smoothly pan and zoom the map to the highlighted marker.
            lng = point_to_highlight[[map_lng_col]],
            lat = point_to_highlight[[map_lat_col]],
            zoom = highlight_zoom
          )
      }
    }
    # If selected_row_indices is empty (e.g., user deselects row by clicking it again
    # if table allows, or selection is cleared programmatically), the clearGroup call
    # at the beginning of this observer handles removing the highlight.
  }, ignoreNULL = FALSE, ignoreInit = TRUE) # ignoreNULL=FALSE: React even when selection is cleared (becomes NULL).
  # ignoreInit=TRUE: Don't run on app start before any selection.
  
  # This function is called for its side effects (setting up observers), so no explicit return value.
  return(invisible(NULL))
}
