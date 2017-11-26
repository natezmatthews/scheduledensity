# Get the data
fc = file(description="workspace/Schedule Density/natezmatthews@gmail.com.ics")
x <- readLines(fc)

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

# For each half hour period of the week, get a list of all the events I've gone to in that time
half.hour.of.week <- function(x) {(x$wday*24*60 + x$hour*60 + x$min) %/% 30}
halfhrs <- list()
for(i in 1:(7*24*2)) {halfhrs[[ i ]] <- df$summary[(half.hour.of.week(df$start) <= i) & (half.hour.of.week(df$end) > i)]}

# Prep for plotting
toplot <- data.frame(cbind(1:(24*7*2),unlist(lapply(halfhrs,length))))
half.hours.possible <- as.integer((Sys.Date() - min(df$startdt))*7*24*2)
toplot[2] <- toplot[2] / half.hours.possible # Convert to % of all half hours in time period
colnames(toplot) <- c("halfhr","cnt")

# Plot!
ggplot(data=toplot, aes(x=halfhr,y=cnt)) + geom_line()

