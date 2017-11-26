fc = file(description="workspace/Schedule Density/natezmatthews@gmail.com.ics")
x <- readLines(fc)
keyval <- do.call(rbind, regmatches(x, regexpr(":", x, fixed = TRUE), invert = TRUE))
keyval <- cbind(keyval,ldply(strsplit(keyval[,1],";TZID="), rbind))
colnames(keyval) <- c("todel","val","key","tz")
keyval$tz <- as.factor(ifelse(is.na(keyval$tz), "UTC", keyval$tz))
keyval[,1] <- NULL
keyval <- keyval[which.max(keyval$key=="BEGIN" & keyval$val=="VEVENT"):tail(which(keyval$key=="END" & keyval$val=="VEVENT"), 1),]
keyval <- cbind.data.frame(keyval, id=cumsum(keyval$key=="BEGIN" & keyval$val=="VEVENT"))
keyval <- keyval[keyval$key %in% c("DTSTART","DTEND","SUMMARY"),]
df <- reshape(keyval, timevar="key","tz"), idvar="id", direction = "wide")
df <- df[complete.cases(df),]
colnames(df) <- c("id","start","end","summary")
