library(shiny)
library(googleVis)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("Hello World!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("icsfile","Choose ics file"),
      uiOutput("date_slider")
    ),
    mainPanel(
      htmlOutput("distPlot")
    )
  )
)