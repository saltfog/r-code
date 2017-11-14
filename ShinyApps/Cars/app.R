#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Relationship between car distance & speed"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     plot(mtcars$hp~mtcars$drat, # y~x
          main="Relationship between car distance & speed", # Plot Title
          xlab="Speed (miles per hour)", #X axis title
          ylab="Distance travelled (miles)", #Y axis title
          #xlim=c(0,30), #Set x axis limits from 0 to 30
          #ylim=c(0,140), #Set y axis limits from 0 to 140
          #xaxs="i", #Set x axis style as internal
          #yaxs="i", #Set y axis style as internal 
          col="red", #Set the color of plotting symbol to red
          pch=19) #Set the plotting symbol to filled dots
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

