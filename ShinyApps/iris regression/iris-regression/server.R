library(shiny)
# Other useful packages
library(datasets)
library(rpart)
library(party)
library(fpc)

# Define colors
palette(c("#E73032", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFDD33", "#A65628", "#F781BF", "#999999"))

# Define server logic 
shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "all iris data" = iris,
           "setosa" = subset(iris, iris$Species == "setosa"),
           "versicolor" = subset(iris, iris$Species == "versicolor"),
           "virginica" = subset(iris, iris$Species == "virginica"))
  })
  
  colX <- reactive({
    switch(input$Xvar,
           "Sepal.Length" = iris$Sepal.Length,
           "Sepal.Width" = iris$Sepal.Width,
           "Petal.Length" = iris$Petal.Length,
           "Petal.Width" = iris$Petal.Width)
  })
  
  colY <- reactive({
    switch(input$Yvar,
           "Sepal.Length" = iris$Sepal.Length,
           "Sepal.Width" = iris$Sepal.Width,
           "Petal.Length" = iris$Petal.Length,
           "Petal.Width" = iris$Petal.Width)
  })
  
  output$NbClust <- renderText({ 
    paste("K-means clustering performed with ", input$clusters," clusters.")
  })
  output$kmeansPlot <- renderPlot({
    plot(iris[,c(input$Xvar,input$Yvar)],
         col = clusters()$cluster,
         pch = 20, cex = 2)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
})