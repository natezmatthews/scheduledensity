# Get the data
fc = file(description="workspace/Schedule Density/natezmatthews@gmail.com.ics")
x <- readLines(fc)
close(fc)

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
df$startdt <- as.Date(df$start,format="%Y%m%dT%H%M%SZ",tz="EST")
df$start <- as.POSIXlt(df$start,format="%Y%m%dT%H%M%SZ",tz="EST")
df$end <- as.POSIXlt(df$end,format="%Y%m%dT%H%M%SZ",tz="EST")

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

# toplot <- cbind(count.matrix,format(
#                                 seq(
#                                       from=as.POSIXct("2017-1-1 0:00"),
#                                       to=as.POSIXct("2017-1-1 23:30"),
#                                       by=60*30
#                                 ),format="%I:%M %p")
#                               )
toplot <- cbind(count.matrix,seq(
                                from=as.POSIXct("2017-1-1 0:00"),
                                to=as.POSIXct("2017-1-1 23:30"),
                                by=60*30
                              )
)
colnames(toplot) <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun","Time")
toplot <- as.data.frame(toplot)
for (i in 1:7) {
  toplot[,i] <- as.numeric(as.character(toplot[,i]))
}
colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
p <- ggplot(toplot,aes(x=Time))
for (i in 1:7) {
  p <- p + geom_line(aes_string(y = colnames(toplot)[i]),colour=colors[i])
}
print(p)

# remove(keyval)
# remove(df)
# remove(events.matrix)
# remove(count.matrix)