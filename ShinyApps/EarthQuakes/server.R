
library(shiny)
library(leaflet)
library(RColorBrewer)
library(RCurl)

m <- getURL('https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.csv')

class(m)
m <- read.csv(textConnection(m), header=T)
head(m)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")
)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(m) %>% addTiles() %>% addMarkers(~longitude, ~latitude, popup = ~as.character(mag), label = ~as.character(mag)) %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
})
