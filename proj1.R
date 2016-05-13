#Code for reading in the dataset and/ord processing the data
data = read.csv("./activity.csv")
data$date = as.Date(as.character(data$date), "%Y-%m-%d")

#data transformation to prepare histogram removing missing values
df = data
stepsPerDay <- with(df, tapply(steps, date, sum, na.rm = T));

df <- data.frame(date = names(stepsPerDay), total = stepsPerDay)
df$date = as.Date(as.character(df$date), "%Y-%m-%d")
df$total = as.numeric(df$total)


#Histogram of the total number of steps taken each day
plot(x = df$date, df$total, type="h", main="Histogram of Daily Steps", xlab="Date", ylab="Steps per Day", col="blue", lwd=5)
abline(h=mean(df$total, na.rm=TRUE), col="red", lwd=2)
abline(h=median(df$total, na.rm=TRUE), col="black", lwd=2)
#Mean and median number of steps taken each day
paste(("Mean number of steps taken each day:"), round(mean(df$total, na.rm = TRUE)))
paste(("Median of number of steps taken each day:"), round(mean(df$total, na.rm = TRUE)))

#Time series plot of the average number of steps taken
dfMnStpsPerInt= data
stepsPerInterval <- with(dfMnStpsPerInt, tapply(steps, interval, mean, na.rm = T));
dfMnStpsPerInt <- data.frame(interval = unique(dfMnStpsPerInt$interval), mean = stepsPerInterval);
plot(x = dfMnStpsPerInt$interval, dfMnStpsPerInt$mean, type="l", main="Time series plot of the average number of\n steps taken in 5 minute Intervals throught out the day", xlab="5 min Intervals", ylab="Steps per Interval", col="red")
abline(h=mean(dfMnStpsPerInt$mean, na.rm=TRUE), col="black", lwd=2)
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
paste("Interval with max value =", dfMnStpsPerInt$interval[which(dfMnStpsPerInt$mean == max(dfMnStpsPerInt$mean))])
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(data$steps))
#Use Mean values per interval to replace missing values
dataImputed = data
for (i in 1:nrow(dataImputed)){
        if (is.na(dataImputed[i,"steps"])){
                dataImputed[i, "steps"] = dfMnStpsPerInt$mean[which(dfMnStpsPerInt$interval == dataImputed[i, "interval"])]; 
        } 
}
sum(is.na(dataImputed$steps))
#Histogram
stepsPerDay <- with(dataImputed, tapply(steps, date, sum, na.rm = T));
dfImputed <- data.frame(date = names(stepsPerDay), total = stepsPerDay)
dfImputed$date = as.Date(as.character(dfImputed$date), "%Y-%m-%d")
dfImputed$total = as.numeric(dfImputed$total)

#Histogram of the total number of steps taken each day
plot(x = dfImputed$date, dfImputed$total, type="h", main="Histogram of Daily Steps", xlab="Date", ylab="Steps per Day", col="blue", lwd=5)
abline(h=mean(dfImputed$total, na.rm=TRUE), col="red", lwd=2)
abline(h=median(dfImputed$total, na.rm=TRUE), col="black", lwd=2)
#Mean and median number of steps taken each day
paste(("Mean number of steps taken each day after imputation:"), round(mean(dfImputed$total)))
paste(("Median of number of steps taken each day after imputation:"), round(mean(dfImputed$total)))
#Are there differences in activity patterns between weekdays and weekends
dataImputed["DayOftheWeek"] <- NA;
for (i in 1:nrow(dataImputed)){
        dataImputed[i, "DayOftheWeek" ] <- weekdays(dataImputed[i, "date"])
        if(dataImputed[i,"DayOftheWeek" ] == "Saturday" | dataImputed[i,"DayOftheWeek" ] == "Sunday"){
                dataImputed[i,"DayOftheWeek" ] = "weekend"
        }
        else {
                dataImputed[i,"DayOftheWeek" ] = "weekday" 
        }

}
dfImputedMnStpsPerIntWd <- subset(dataImputed, DayOftheWeek == "weekday")
stepsPerInterval <- with(dfImputedMnStpsPerIntWd, tapply(steps, interval , mean, na.rm = T))
dfImputedMnStpsPerIntWd <- data.frame(interval = unique(dfImputedMnStpsPerIntWd$interval), mean = stepsPerInterval);

dfImputedMnStpsPerIntWE <- subset(dataImputed, DayOftheWeek == "weekend")
stepsPerInterval <- with(dfImputedMnStpsPerIntWE, tapply(steps, interval , mean, na.rm = T))
dfImputedMnStpsPerIntWE <- data.frame(interval = unique(dfImputedMnStpsPerIntWE$interval), mean = stepsPerInterval);


par(mfrow=c(2,1)) 
plot(x = dfImputedMnStpsPerIntWd$interval, dfImputedMnStpsPerIntWd$mean, type="l", main="Time series plot of the average number of\n steps taken in 5 minute Intervals on weekdays", xlab="5 min Intervals", ylab="Steps per Interval", col="red")
plot(x = dfImputedMnStpsPerIntWE$interval, dfImputedMnStpsPerIntWE$mean, type="l", main="Time series plot of the average number of\n steps taken in 5 minute Intervals on weekdays", xlab="5 min Intervals", ylab="Steps per Interval", col="red")
