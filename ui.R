library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("Hello World!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("icsfile","Choose ics file"),
      sliderInput("date_range", 
                  "Choose Date Range:", 
                  min = as.Date("1998-01-01"), # The year iCalendar was invented
                  max = Sys.Date(),
                  value = c(as.Date("2017-01-01"),Sys.Date())
      )
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)