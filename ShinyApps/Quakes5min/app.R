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
  tags$h4(
  tags$div(class="text-center", "Earth Quakes - Past Hour Updated every 5 minutes")),
  tags$h6(
  tags$div(class="text-center", "Data Source: https://earthquake.usgs.gov")),
  leafletOutput("map", width = "100%", height = "100%")
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(m) %>% addTiles() %>% addMarkers(~longitude, ~latitude, popup = ~as.character(mag), label = ~as.character(mag)) %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
}

shinyApp(ui, server)
