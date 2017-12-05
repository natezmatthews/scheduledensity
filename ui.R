library(shiny)
library(googleVis)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("When am I usually free?"),
  
  htmlOutput("distPlot"),
  
  hr(),
  
  fluidRow(
    column(12,
      fileInput("icsfile","Choose ics file"),
      uiOutput("date_slider")
    )
  )
)