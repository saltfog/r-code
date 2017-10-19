library(shiny)
library(leaflet)
library(RColorBrewer)
library(RCurl)
library(htmltools)

m <- getURL('https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_day.csv')

class(m)
m <- read.csv(textConnection(m), header=T)
head(m)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$h4(
  tags$div(class="text-center", "Earth Quakes - Past 24hrs Updated every 5 minutes")),
  tags$h6(
  tags$div(class="text-center", "Data Source: https://earthquake.usgs.gov")),
  leafletOutput("map", width = "100%", height = "100%")
)

server <- function(input, output, session) {
  outline <- m[chull(m$longitude, m$latitude),]
  
  output$map <- renderLeaflet({
    leaflet(m) %>% addTiles() %>% addCircles(~longitude, ~latitude, ~10^mag*10, color = "blue", group = "Quakes", popup = ~as.character(mag)) %>%
       #addPopups(~longitude, ~latitude, ~as.character(mag)) %>%
      addPolygons(data = outline, lng = ~longitude, lat = ~latitude,
                  fill = F, weight = 2, color = "white", group = "Outline") %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
      addLayersControl(
        overlayGroups = c("Quakes", "Outline"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

shinyApp(ui, server)
