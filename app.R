# Load packages
library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(shinythemes)
library(dplyr)
library(rsconnect)

# Load data
crime <- readOGR(dsn = "data", layer = "NIJ_Nov2016_Crime")
crime <- spTransform(crime, CRS("+init=epsg:4326"))
crime$occ_date <- as.Date(crime$occ_date)

# --------------------
# Define UI
# --------------------

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("NIJ Crimes"),
  leafletOutput("mymap"),
  p(),
  selectInput("cat", "Choose a Crime Type:",as.vector(unique(crime$CATEGORY))),
  dateInput("date1", "Start Date:", value = "2016-11-01"),
  dateInput("date2", "End Date:",value = "2016-11-30")
)

# --------------------
# Define Server
# --------------------

server <- function(input, output, session) {
  
  filteredData <- reactive({
    crime[crime$CATEGORY==input$cat & crime$occ_date>as.Date(input$date1) &
            crime$occ_date<as.Date(input$date2),]
  })
  
  # Create the Map
  output$mymap <- renderLeaflet({
    crime %>% leaflet() %>% addTiles() %>%
      fitBounds(bbox(crime)[1,1], bbox(crime)[2,1],bbox(crime)[1,2], bbox(crime)[2,2])
  })
  
  # Create Circles
  observe({
    leafletProxy("mymap",data=filteredData()) %>% clearShapes() %>% addCircles()
  })
  
}

# --------------------
# Launch App
# --------------------

shinyApp(ui, server)
#rsconnect::deployApp()

