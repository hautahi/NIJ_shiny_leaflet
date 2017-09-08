# Load packages
library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(shinythemes)
library(dplyr)

# Load data
d <- read.csv("./data/NIJ.csv",stringsAsFactors = F)
d$date <- as.Date(d$date)

# --------------------
# Define UI
# --------------------

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Crime Locations in Portland, Oregon"),
  leafletOutput("mymap"),
  p(""),
  helpText("This application allows the user to specify a crime type and 
           a time interval to display the locations of crimes in Portland, Oregon."),
  fluidRow(
    column(width = 4,
           selectInput("cat", "Choose a Crime Type:",as.vector(unique(d$category)))
    ),
    column(width = 3, offset = 2,
           dateRangeInput('date',
                          label = 'Date range:',
                          start = "2017-05-01", 
                          end = "2017-05-31",
                          min = min(d$date),
                          max = max(d$date)
           )))
)

# --------------------
# Define Server
# --------------------

server <- function(input, output, session) {
  
  filteredData <- reactive({
    d %>% filter(category==input$cat,date>=input$date[1],
                 date<input$date[2])
  })
  
  # Create the Map
  output$mymap <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      fitBounds(min(d$lon),min(d$lat),max(d$lon),max(d$lat))
  })
  
  # Create Circles
  observe({
    leafletProxy("mymap",data=filteredData()) %>% clearShapes() %>%
      addCircles(~lon,~lat)
  })
  
}

# --------------------
# Launch App
# --------------------

shinyApp(ui, server)

# --------------------
# Deploy App
# --------------------

#rsconnect::deployApp()
