#Code for reading in the dataset and/or processing the data
data = read.csv("./activity.csv")

df = na.omit(data)
stepsPerDay <- with(df, tapply(steps, date, sum, na.rm = T));
df <- data.frame(date = names(stepsPerDay), total = stepsPerDay)
df$date = as.Date(as.character(df$date), "%Y-%m-%d")
df$total = as.numeric(df$total)
df = na.omit(df)

plot(x = df$date, df$total, type="h", main="Histogram of Daily Steps", xlab="Date", ylab="Steps per Day", col="blue", lwd=5)
abline(h=mean(df$total, na.rm=TRUE), col="red", lwd=2)
abline(h=median(df$total, na.rm=TRUE), col="black", lwd=2)

paste(("Mean number of steps taken each day:"), round(mean(df$total)))
paste(("Median of number of steps taken each day:"), round(mean(df$total)))
S