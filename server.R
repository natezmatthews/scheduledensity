library(shiny)
# library(ggplot2)
library(googleVis)
library(scales)
fc = file(description="~/workspace/Schedule Density/natezmatthews@gmail.com.ics")
x <- readLines(fc)
close(fc)

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
  df$start <- as.POSIXct(df$start,format="%Y%m%dT%H%M%SZ",tz="UTC") # POSIXlt for wkday
  df$end <- as.POSIXct(df$end,format="%Y%m%dT%H%M%SZ",tz="UTC") # POSIXlt for wkday
  df$start <- as.POSIXlt(df$start,tz="EST") # POSIXlt for wkday
  df$end <- as.POSIXlt(df$end,tz="EST") # POSIXlt for wkday

  return(df)  
}

weekday.plot <- function(df,fromdt,todt) {
  # Restrict to the daterange from the slider
  if (!is.null(fromdt) & !is.null(todt)) {
    df <- df[as.POSIXct(fromdt) <= df$start & as.POSIXct(todt) > df$end,] 
  }
  
  half.hour.of.day <- function(x) {(x$hour*60 + x$min) %/% 30}
  half.hours.possible <- as.integer(as.POSIXct(Sys.Date()) - min(df$start))/7
  week.days <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
  tool.tips = unlist(lapply(week.days,function(x){paste(x,"html","tooltip",sep=".")}))
  toplot <- data.frame(Mon=numeric(24*2),
                       Mon.html.tooltip=character(24*2),
                       Tue=numeric(24*2),
                       Tue.html.tooltip=character(24*2),
                       Wed=numeric(24*2),
                       Wed.html.tooltip=character(24*2),
                       Thu=numeric(24*2),
                       Thu.html.tooltip=character(24*2),
                       Fri=numeric(24*2),
                       Fri.html.tooltip=character(24*2),
                       Sat=numeric(24*2),
                       Sat.html.tooltip=character(24*2),
                       Sun=numeric(24*2),
                       Sun.html.tooltip=character(24*2),
                       stringsAsFactors=FALSE)
  for (j in 1:7) {
    for(i in 1:(24*2)) {
      event.list <- df$summary[(half.hour.of.day(df$start) <= i)
                               & (half.hour.of.day(df$end) > i)
                               & (df$start$wday == j-1)]
      toplot[[week.days[j]]][i] <- length(event.list) / half.hours.possible
      toplot[[tool.tips[j]]][i] <- paste0(event.list,collapse="<br>")
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
  mydf <- parse.data(x)
  
  output$date_slider <- renderUI({
    inFile <- input$icsfile
    if (!is.null(inFile)) {
      mydf <- parse.data(readLines(inFile$datapath))
    }
    dateRangeInput("date_range",
                   "Date Range",
                   start = min(mydf$start),
                   end = max(mydf$end)
                   )
  })
  
  output$distPlot <- renderGvis({
    inFile <- input$icsfile
    if (!is.null(inFile)) {
      mydf <- parse.data(readLines(inFile$datapath))
    }
    toplot <- weekday.plot(mydf,input$date_range[1],input$date_range[2])
    week.days <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
    tool.tips = unlist(lapply(week.days,function(x){paste(x,"html","tooltip",sep=".")}))
    yvarcols = as.vector(t(cbind(week.days,tool.tips)))
    gvisLineChart(toplot,xvar="Time",yvar=yvarcols,
                        options=list(vAxis="{title:'% weeks with plans',
                                     format:'#,###%'}",
                                     tooltip="{isHtml:'true'}",
                                     height=400))
  })
}