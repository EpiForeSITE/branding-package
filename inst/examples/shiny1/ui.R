library(bslib)
library(yaml)

# Create a theme object
theme <- bslib::bs_theme(brand = TRUE)

# Extract the path of the discovered _brand.yml file
brand_info <- attr(theme, "brand")
brand_path <- brand_info$path


# Read YAML file
brand <- yaml.load_file(brand_path)
dput(brand)
brand_colors <- brand$color
dput(brand_colors)

ui <- bslib::page_sidebar(
    # This fixes a common accessibility issue with shiny apps
    tags$head(HTML("<html lang='en'>")),
    theme = bs_theme(),

  title = tags$h1("Simple Shiny"),

  sidebar = bslib::sidebar(
    theme = "brand_colors$primary",
    tags$h2("Sidebar"),
    numericInput(
      "num1",
      "Mean",
      value = 50,
      min = 1,
      max = 100
    ),
    card(
      title = "Card",


      style = paste("background-color:", brand_colors$primary),

      tags$h3("Card Title"),
      tags$p(brand_colors$primary))

    ),

  plotOutput("hist")
)