# Returns a filepath to a temporary yaml file
# brand_dhhs <- function() {load_brand("brand/_brand2.yml", color.main = "blue")}

#  ui.page_opts(theme=ui.Theme.from_brand("brand/_gvy_brand.yml"))

#' @export
run_model <- function() {
	shiny::runApp('.' )
}

run_model()
