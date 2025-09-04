# UI -----

## Create header -----

header <- dashboardHeader(title = "", #Replace with your title (displayed in the dashboard)
           titleWidth = 200, #Can change to accommodate your title
           tags$li(a(href = "https://posit-dev.github.io/brand-yml/",
               img(src = "icon.png",
                 title = "rbranding package",
                 style = "height:35px;width:35px;"),
               style = "padding-top: 5px; padding-bottom: 5px;"),
                class = "dropdown"))


## Define UI -----

ui <- dashboardPage(
  
  # This sets the title in the browser tab
  title = "Shiny App Example 1",
  
  # Implement the header created above
  header,
  
  ## Navigation sidebar -----
  
  # More icons can be found at "fontawesome.com/icons" and "getbootstrap.com/docs/3.4/components/#glyphicons" 
  # Note that some may not work, so test first!
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      ### 1st tab -----
      menuItem("Tab 1", tabname = "tab1", icon = icon("chart-line")),
      
      ### 2nd tab -----
      menuItem("Tab 2", tabname = "tab2", icon = icon("stats", lib = "glyphicon")),
      
      ### Slider -----
      uiOutput("slider")
    ) #Close sidebarMenu
  ), #Close dashboardSidebar
  
  ## Dashboard body -----
  dashboardBody(
    
    # Import CSS
   
    
    ### 1st tab -----
    tabItem(
      tabName = "Tab 1",
      
      #Tab 1 contents
      fluidRow(
        #Static infobox (there are also dynamic infoboxes that work similarly to valueboxes)
        infoBox("infobox1",
                34,
                icon = icon("list"),
                width = 2),
        
        #Dynamic valuebox
        valueBoxOutput("valuebox1", width = 2)
      )
    ), #Close Tab 1
    
    ### 2nd tab -----
    tabItem(
      tabName = "Tab 2",
      
      #Tab 2 contents
      fluidRow(
        box(title = "Old Faithful Geyser Data",
            plotOutput("distplot"))
      )
      
    ), #Close Tab 2
    
    ) #Close dashboardBody
  
) #close dashboardPage