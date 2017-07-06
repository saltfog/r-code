#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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