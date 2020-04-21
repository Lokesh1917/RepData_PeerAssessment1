if(!file.exists('activity.csv')){
unzip('activity.zip')
}
actData <- read.csv('activity.csv')

ttlsteps <- tapply(actData$steps, actData$date, FUN=sum, na.rm=TRUE)
qplot(ttlsteps, binwidth=500, xlab=" number of steps ",ylab = "Frequency",main = "Total Steps Each Day")
 
mean(ttlsteps)
median(ttlsteps)


avg <- aggregate(x=list(steps=actData$steps), by=list(interval=actData$interval),FUN=mean, na.rm=TRUE)
ggplot(data=avg, aes(x=interval, y=steps)) +geom_line() +labs(x="5-minute interval",y="average number of steps taken")


missvals <- length(is.na(actData$steps))
##function to fill
fill <- function(steps, interval) {
f <- NA
if (!is.na(steps))
f <- c(steps)
else
f <- (avg[avg$interval==interval, "steps"])
return(f)
}
## 
actDataImp <- actData
actDataImp$steps <- mapply(fill,actDataImp$steps,actDataImp$interval)


ttlsteps <- tapply(actDataImp$steps, actDataImp$date, FUN=sum)
hist(ttlsteps, binwidth=500, xlab=" number of steps (completed) ",ylab = "Frequency",main = "Total Steps Each Day")

mean(ttlsteps)
median(ttlsteps)

actDataImp$date <- as.Date(actDataImp$date)
actDataImp$weekday <- weekdays(actDataImp$date)
actDataImp$weekend <- ifelse(actDataImp$weekday=="Saturday" | actDataImp$weekday=="Sunday", "Weekend", "Weekday" )

wkendnday <- aggregate(actDataImp$steps , by= list(actDataImp$weekend, actDataImp$interval), na.omit(mean))
ggplot(wkendnday, aes(x=interval, y=steps, color=weekend)) + geom_line()+facet_grid(weekend ~.) + labs(x="Interval", y="Mean Of steps")+ggtitle("Comparison of AvgNo of Steps ")


