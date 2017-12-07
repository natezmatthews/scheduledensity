library(shiny)
library(googleVis)

# Define UI for application that draws a histogram
fluidPage(
  
  theme="bootstrap.css",
  
  # Application title
  titlePanel(fluidRow(
                column(12,
                       align="center",
                       "When do I usually have plans?"
                       )
  )),
  
  htmlOutput("distPlot"),
  
  hr(),
  
  fluidRow(
    column(12,
      align="center",
      fileInput("ics_file","Choose ics file"),
      uiOutput("date_slider"),
      selectInput("timezone_dropdown",
                  "Change time zone",
                  OlsonNames()
                  )
    )
  )
)