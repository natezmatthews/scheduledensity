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
  
  # Convert to datatypes I can use
  df$summary <- lapply(df$summary, as.character)
  df$start <- as.POSIXct(df$start,format="%Y%m%dT%H%M%SZ",tz="UTC") # POSIXlt for wkday
  df$end <- as.POSIXct(df$end,format="%Y%m%dT%H%M%SZ",tz="UTC") # POSIXlt for wkday
  df$start <- as.POSIXlt(df$start,tz=input_tz) # POSIXlt for wkday
  df$end <- as.POSIXlt(df$end,tz=input_tz) # POSIXlt for wkday

  return(df)  
}

weekday.plot <- function(df,fromdt,todt) {
  # Restrict to the daterange from the slider
  if (!is.null(fromdt) & !is.null(todt)) {
    not.too.early <- as.POSIXct(fromdt) <= df$start
    not.too.late <- as.POSIXct(todt) > df$end 
    df <- df[not.too.early & not.too.late,]
  }
  
  half.hour.of.day <- function(x) {(x$hour*60 + x$min) %/% 30}
  half.hours.possible <- as.integer(as.POSIXct(Sys.Date()) - min(df$start))/7
  week.days <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
  half.hours.per.day <- 24*2
  
  tool.tips = unlist(lapply(week.days,function(x){paste(x,"html","tooltip",sep=".")}))
  toplot <- data.frame(Sun=numeric(half.hours.per.day),
                       Sun.html.tooltip=character(half.hours.per.day),
                       Mon=numeric(half.hours.per.day),
                       Mon.html.tooltip=character(half.hours.per.day),
                       Tue=numeric(half.hours.per.day),
                       Tue.html.tooltip=character(half.hours.per.day),
                       Wed=numeric(half.hours.per.day),
                       Wed.html.tooltip=character(half.hours.per.day),
                       Thu=numeric(half.hours.per.day),
                       Thu.html.tooltip=character(half.hours.per.day),
                       Fri=numeric(half.hours.per.day),
                       Fri.html.tooltip=character(half.hours.per.day),
                       Sat=numeric(half.hours.per.day),
                       Sat.html.tooltip=character(half.hours.per.day),
                       stringsAsFactors=FALSE)
  for (j in 1:7) {
    for(i in 1:(half.hours.per.day)) {
      event.list <- df$summary[(half.hour.of.day(df$start) <= i)
                               & (half.hour.of.day(df$end) > i)
                               & (df$start$wday == j-1)]
      
      if (length(event.list) > 0) {
        toplot[[week.days[j]]][i] <- length(event.list) / half.hours.possible
        tooltip.string <- paste0(event.list,collapse="<br>")
      } else { # There are no events for this half hour of this weekday
        # If GoogleVis gets the empty string it will display its default tootlip,
        # which doesn't make sense for our application. Let's make it a space instead.
        tooltip.string <- " "
      }
      toplot[[tool.tips[j]]][i] <- tooltip.string
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