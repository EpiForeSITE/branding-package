#
# DHHS R Shiny Template
#
# You can run the application by clicking the 'Run App' button above.
#


# Library & functions -----
library(shiny)
library(shinydashboard)
library(htmltools)
library(bslib)
library(shinyWidgets)
library(plotly)
library(leaflet)
library(janitor)
library(lubridate)
library(tidyverse)

run_model <- function() {
	shiny::runApp('.' )
}

run_model()
