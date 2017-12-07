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
             )
  ),
  
  fluidRow(
    column(2),
    column(8,
           align="justify",
           br(),
           p("When you're scheduling something, it can be good to know when you're typically busy. 
              When you first visit this page you will see the chart generated from my calendar file.
              Try exporting your calendar to the common ICS format, and uploading that ICS file here.
              Hover over the lines to see the names of the events that you had at that time and on 
              that day of the week. If instead of looking at your whole calendar you would like to 
              focus on events from a particular time period, use the date slider. Change the timezone 
              dropdown to adjust the times to your time zone, and let me know if it looks off; 
               timezones are tricky."),
           br()
    ),
    column(2)
  ),
  
  fluidRow(
    column(4,
           align="center",
           uiOutput("date_slider")
    ),
    column(4,
             align="center",
             fileInput("ics_file","Choose ics file")
    ),
    column(4,
           align="center",
           selectInput("tz_dropdown",
                       "Change time zone",
                       OlsonNames(),
                       selected="EST"
           )
    )
  ),
  
  hr(),
  
  htmlOutput("distPlot")
)