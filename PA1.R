unzip(zipfile="activity.zip")
rawdata <- read.csv("activity.csv")
attach(rawdata)
daily<- aggregate(steps,by=list(date),FUN=sum)
#daily<-aggregate(rawdata$steps, list(rawdata$date), sum)
detach(rawdata)
names(daily)<-c("day","steps")
#totalsteps <- data.frame(tapply(rawdata$steps, rawdata$date, FUN=sum, na.rm=TRUE))
#hist(daily$steps,na.rm=TRUE,breaks=61)
library(ggplot2)
qplot(daily$steps, binwidth = 1000, xlab = "Total Steps/day")
mean(daily$steps,na.rm = TRUE)
median(daily$steps,na.rm = TRUE)
#Average across days per interval
attach(rawdata)
intervals<-aggregate(steps,by=list(interval),mean,na.rm=TRUE)
detach(rawdata)
names(intervals)<-c("Interval","Avgsteps")
#Plotting a line chart
ggplot(data = intervals, aes(x = Interval, y = Avgsteps)) + geom_line() + xlab("5-minute interval") + 
  ylab("Avg. # of steps taken")
intervals[which.max(intervals$Avgsteps), ]
missing <- sum(is.na(rawdata$steps))
missing
impute <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps)) 
    filled <- c(steps) else filled <- (intervals[intervals$interval == interval, "steps"])
    return(filled)
}
names(intervals)<-c("interval","steps")
rawdata_filled<-rawdata
rawdata_filled$steps<-mapply(impute,rawdata_filled$steps,rawdata_filled$interval)

attach(rawdata_filled)
daily_filled<- aggregate(steps,by=list(date),FUN=sum)
detach(rawdata_filled)
names(daily_filled)<-c("day","steps")
qplot(daily_filled$steps, binwidth = 1000, xlab = "Total Steps/day")

#Determine Weekend or Weekday
wday<- function(date)
  {
  library(chron)
  if (is.weekend(date))
  return ("weekend")
else 
  return ("weekday")
}

rawdata_filled$date <-as.Date(rawdata_filled$date)
rawdata_filled$day<- sapply(rawdata_filled$date,FUN = wday)
#Plotting Intervals between Weekend and weekday.More rest on weekend:)
averages <- aggregate(steps ~ interval + day, data = rawdata_filled, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
  xlab("5-minute interval") + ylab("Number of steps")

##KNITTING HTML

library(knitr)
rmarkdown::render("PA1_template.Rmd")



