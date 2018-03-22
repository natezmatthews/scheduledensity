library(shiny)
library(googleVis)
library(scales)
fc = file(description="www/natezmatthews@gmail.com.ics")
x <- readLines(fc)
close(fc)

parse.data <- function(x,input_tz) {
  # Turn it into a nice dataframe with a row per event I can use
  keyval <- do.call(rbind, regmatches(x, regexpr(":", x, fixed = TRUE), invert = TRUE))
  keyval <- keyval[which.max(keyval[,1]=="BEGIN" & keyval[,2]=="VEVENT"):tail(which(keyval[,1]=="END" & keyval[,2]=="VEVENT"), 1),]
  keyval <- cbind.data.frame(keyval, id=cumsum(keyval[,1]=="BEGIN" & keyval[,2]=="VEVENT"))
  keyval <- keyval[keyval[,1] %in% c("DTSTART","DTEND","SUMMARY"),]
  df <- reshape(keyval, timevar="1", idvar="id", direction = "wide")
  df <- df[complete.cases(df),]
  
  # Nicer names to work with in my opinion
  colnames(df)[colnames(df)=="2.DTSTART"] <- "start"
  colnames(df)[colnames(df)=="2.DTEND"] <- "end"
  colnames(df)[colnames(df)=="2.SUMMARY"] <- "summary"
  
  df$summary <- lapply(df$summary, as.character)
  
  # String to date conversion, using UTC
  utc.start <- as.POSIXct(df$start,format="%Y%m%dT%H%M%SZ",tz="UTC")
  utc.end <- as.POSIXct(df$end,format="%Y%m%dT%H%M%SZ",tz="UTC")
  # Convert to the input timezone. I use POSIXlt for easy lookup of the day of the week
  df$start <- as.POSIXlt(utc.start,tz=input_tz)
  df$end <- as.POSIXlt(utc.end,tz=input_tz)

  return(df)  
}

weekday.plot <- function(df,fromdt,todt) {
  # Restrict to the daterange from the slider
  if (!is.null(fromdt) & !is.null(todt)) {
    not.too.early <- as.POSIXct(fromdt) <= df$start
    not.too.late <- as.POSIXct(todt) > df$end 
    df <- df[not.too.early & not.too.late,]
  }
  
  # Function to give you which half hour of the day the input was,
  # as an integer, i.e. was it the first half hour of the day? The fifth? Etc
  half.hour.of.day <- function(x) {(x$hour*60 + x$min) %/% 30}
  
  # How many half hours _could_ you have had something scheduled in?
  half.hours.possible <- as.integer(max(df$end) - min(df$start))/7
  
  week.days <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
  half.hours.per.day <- 24*2
  
  # This will give us Sun.html.tooltip, Mon.html.tooltip, etc, for Google Vis:
  tool.tips = unlist(lapply(week.days,function(x){paste(x,"html","tooltip",sep=".")}))
  
  # Initialize the dataframe with a zero and a space character for each half hour of the day
  # I use a space instead of an empty string because if GoogleVis gets the empty string 
  # it will display its default tootlip, which doesn't make sense for our application.
  zeroes <- rep(0,half.hours.per.day)
  spaces <- rep(" ",half.hours.per.day)
  toplot <- data.frame(Sun=zeroes,
                       Sun.html.tooltip=spaces,
                       Mon=zeroes,
                       Mon.html.tooltip=spaces,
                       Tue=zeroes,
                       Tue.html.tooltip=spaces,
                       Wed=zeroes,
                       Wed.html.tooltip=spaces,
                       Thu=zeroes,
                       Thu.html.tooltip=spaces,
                       Fri=zeroes,
                       Fri.html.tooltip=spaces,
                       Sat=zeroes,
                       Sat.html.tooltip=spaces,
                       stringsAsFactors=FALSE)
  for (j in 1:7) {
    for(i in 1:half.hours.per.day) {
      event.list <- df$summary[(half.hour.of.day(df$start) <= i)
                               & (half.hour.of.day(df$end) > i)
                               & (df$start$wday == j-1)]
      
      if (length(event.list) > 0) {
        # What % of half hours available had an event scheduled?
        toplot[[week.days[j]]][i] <- length(event.list) / half.hours.possible
        # What were the names of those events?
        toplot[[tool.tips[j]]][i] <- paste0(event.list,collapse="<br>")
      }
    }
  }
  toplot <- cbind(toplot,Time=seq(
                    from=as.POSIXct("2017-1-1 0:00"),
                    to=as.POSIXct("2017-1-1 23:30"),
                    by=60*30
                  )
  )
  return(toplot)
}

function(input, output) {
  output$date_slider <- renderUI({
    inFile <- input$ics_file
    if (!is.null(inFile)) {
      mydf <- parse.data(readLines(inFile$datapath),input$tz_dropdown)
    } else {
      mydf <- parse.data(x,input$tz_dropdown)
    }
    dateRangeInput("date_range",
                   "Restrict the date range",
                   start = min(mydf$start),
                   end = max(mydf$end)
                   )
  })
  
  output$distPlot <- renderGvis({
    inFile <- input$ics_file
    if (!is.null(inFile)) {
      mydf <- parse.data(readLines(inFile$datapath),input$tz_dropdown)
    } else {
      mydf <- parse.data(x,input$tz_dropdown)
    }
    toplot <- weekday.plot(mydf,input$date_range[1],input$date_range[2])
    week.days <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
    tool.tips = unlist(lapply(week.days,function(x){paste(x,"html","tooltip",sep=".")}))
    yvarcols = as.vector(t(cbind(week.days,tool.tips)))
    gvisLineChart(toplot,xvar="Time",yvar=yvarcols,
                        options=list(vAxis="{title:'% of weeks that had plans',
                                     format:'#,###%'}",
                                     tooltip="{isHtml:'true'}",
                                     height=450))
  })
}