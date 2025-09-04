
# Server -----
server <- function(input, output) {
  
  output$slider <- renderUI({
    sliderInput("bins",
                "Number of bins:",
                min = 1,
                max = 50,
                value = 30)
  })

  ## 1st tab -----
  
  output$valuebox1 <- renderValueBox({
    valueBox(7 ^ 3,
             subtitle = "Valuebox 1",
             icon = icon("map-location-dot"),
             color = "orange")
  })
  
  ## 2nd tab -----
  
    output$distplot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}