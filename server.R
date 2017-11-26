library(shiny)
library(ggplot2)
library(scales)
fc = file(description="~/workspace/Schedule Density/natezmatthews@gmail.com.ics")
x <- readLines(fc)

parse.data <- function(x) {
  # Turn it into a nice dataframe with a row per event I can use
  keyval <- do.call(rbind, regmatches(x, regexpr(":", x, fixed = TRUE), invert = TRUE))
  keyval <- keyval[which.max(keyval[,1]=="BEGIN" & keyval[,2]=="VEVENT"):tail(which(keyval[,1]=="END" & keyval[,2]=="VEVENT"), 1),]
  keyval <- cbind.data.frame(keyval, id=cumsum(keyval[,1]=="BEGIN" & keyval[,2]=="VEVENT"))
  keyval <- keyval[keyval[,1] %in% c("DTSTART","DTEND","SUMMARY"),]
  df <- reshape(keyval, timevar="1", idvar="id", direction = "wide")
  df <- df[complete.cases(df),]
  colnames(df) <- c("id","start","end","summary")
  
  # Convert to datatypes I can use
  df$summary <- lapply(df$summary, as.character)
  df$startdt <- as.Date(df$start,format="%Y%m%dT%H%M%SZ",tz="EST") # Date for timediff and comparisons
  df$enddt <- as.Date(df$start,format="%Y%m%dT%H%M%SZ",tz="EST") # Date for timediff and comparisons
  df$start <- as.POSIXlt(df$start,format="%Y%m%dT%H%M%SZ",tz="EST") # POSIXlt for wkday
  df$end <- as.POSIXlt(df$end,format="%Y%m%dT%H%M%SZ",tz="EST") # POSIXlt for wkday

  return(df)  
}

prep.for.plot <- function(df,fromdt,todt) {
  # Restrict to the daterange from the slider
  df <- df[fromdt <= df$startdt & todt > df$enddt,]
  
  # For each half hour period of the week, get a list of all the events I've gone to in that time
  half.hour.of.week <- function(x) {(x$wday*24*60 + x$hour*60 + x$min) %/% 30}
  halfhrs <- list()
  for(i in 1:(7*24*2)) {halfhrs[[ i ]] <- df$summary[(half.hour.of.week(df$start) <= i) & (half.hour.of.week(df$end) > i)]}
  
  # Prep for plotting
  toplot <- data.frame(cbind(1:(7*24*2),unlist(lapply(halfhrs,length))))
  half.hours.possible <- as.integer(todt - min(df$startdt))/7
  toplot[2] <- toplot[2] / half.hours.possible # Convert to % of all half hours in time period
  colnames(toplot) <- c("halfhr","cnt")
  return(toplot)
}

weekday.plot <- function(df,fromdt,todt) {
  # Restrict to the daterange from the slider
  df <- df[fromdt <= df$startdt & todt > df$enddt,]
  
  half.hour.of.day <- function(x) {(x$hour*60 + x$min) %/% 30}
  events.matrix <- matrix(list(),nrow=24*2,ncol=7)
  count.matrix <- matrix(0,nrow=24*2,ncol=7)
  for(j in 1:7) {
    for(i in 1:(24*2)) {
      events.matrix[i,j] <- list(df$summary[(half.hour.of.day(df$start) <= i)
                                            & (half.hour.of.day(df$end) > i)
                                            & (df$start$wday == j-1)])
      count.matrix[i,j] <- lengths(events.matrix[i,j])
    }
  }
  half.hours.possible <- as.integer(Sys.Date() - min(df$startdt))/7
  
  toplot <- as.data.frame(count.matrix / half.hours.possible)
  toplot <- cbind(toplot,seq(
    from=as.POSIXct("2017-1-1 0:00"),
    to=as.POSIXct("2017-1-1 23:30"),
    by=60*30
  )
  )
  colnames(toplot) <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun","Time")
  for (i in 1:7) {
    toplot[,i] <- as.numeric(as.character(toplot[,i]))
  }
  return(toplot)
}

# Define server logic required to draw a histogram
function(input, output) {
  rct <- reactive(inFile <- input$icsfile)
  # if (is.null(inFile)) {
  #   return(NULL)
  # }
  # x <- readLines(inFile)
  
  mydf <- parse.data(x)
  
  output$date_slider <- renderUI({
    dateRangeInput("date_range",
                   "Date Range",
                   start = min(mydf$startdt),
                   end = max(mydf$enddt)
                   )
  })
  
  output$distPlot <- renderPlot({
    # Plot!
    toplot <- weekday.plot(mydf,input$date_range[1],input$date_range[2])
    colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    p <- ggplot(toplot,aes(x=Time)) + 
      scale_x_datetime("", date_labels = "%I:%M %p") +
      scale_y_continuous(labels=percent) +
      ylab("% of weeks with plans")
    for (i in 1:7) {
      p <- p + geom_line(aes_string(y = colnames(toplot)[i]),colour=colors[i])
    }
    return(p)
  })
}