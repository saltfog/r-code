#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


library(shiny)
ui <- fluidPage(
  fluidRow(
    column(4,
           wellPanel(
             sliderInput("sampleSize","Please Select Sample Size:",
                         min = 0,max = 5000,value = 1000,step = 100)),
             uiOutput("invar"),
             uiOutput("outvar"),
             uiOutput("moC"))
    ),

  
  
  mainPanel(
    plotOutput("histogram")
  )
)
server <- function(input, output) {
  
  sorted <-  reactive({
    data <- iris[ ,c(input$inv)]
    data})
  
  output$invar<-renderUI({
    selectizeInput('invar',"Select Regression Input Variables", choices = names(data), multiple = TRUE)
  })
  
  output$outvar<-renderUI({
    selectizeInput('outvar',"Select Regression Output Variable", choices = names(data), multiple = TRUE)
    
  })
  
  
  
  
  sorted <-  reactive({
    iris[input$invar]
  })
  
  output$moC <- renderUI({
    numvar<- length(input$invar)
    
    lapply(1:numvar, function(i) {
      tagList(
        selectInput("inv",paste0("Please Select Probability Distribution of ", input$invar[i]),
                    choices = c("Normal","Uniform")),
        conditionalPanel(condition = "input.inv=='Normal'",
                         textInput("invarpdfmean","Please Select Input Variable Mean:",0.25),
                         textInput("invarpdfsd","Please Select Input Variable Standard Deviation", 0.02)),
        conditionalPanel(condition = "input.inv=='Uniform'",
                         textInput("invarpdfmin","Please Select Minimum Input Variable Value:",0.18),
                         textInput("invarpdfmax","Please Select Maximum Input Variable Value", 0.3))
        
        
      )})})
  
  
  output$histogram <- renderPlot({
    n <- input$sampleSize
    
    
    
    if(input$invar[i]=="Normal"){
      
      invarpdfVec <- rnorm(n,mean = as.numeric(input$invarpdfmean),sd= as.numeric(input$invarpdfsd))
    }
    
    if(input$invar[i]=="Uniform"){
      
      invarpdfVec <- runif(n,min = as.numeric(input$invarpdfmin),max = as.numeric(input$invarpdfmax))
    }
    
    MCtab <- iris[ , input$invar]
    for (n in 1:input$sampleSize){
      h <- (invarpdfVec+invarpdfVec)
    }
    
    h<- hist(h,breaks=30,col="red",freq=F)
    # Define the quartiles for the Min, Max, P10, P50 and P90
    
  })
}
    

shinyApp(ui, server)

