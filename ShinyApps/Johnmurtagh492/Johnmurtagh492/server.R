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
    
    sorted <-  reactive({
      data <- iris[ ,c(input$inv)]
      data})
    
    output$invar<-renderUI({
      selectizeInput('invar',"Select Regression Input Variables", names(iris) , multiple = TRUE)
    })
    
    output$outvar<-renderUI({
      selectizeInput('outvar',"Select Regression Output Variable", names(iris) , multiple = TRUE)
      
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
})