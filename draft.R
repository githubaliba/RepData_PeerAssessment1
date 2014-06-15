## parameterize directory, create data directory if needed
dataDir <- "./data"
if(!file.exists(dataDir)) {dir.create(dataDir)}

## parameterize URL, filename, download file
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destFileName <- "activity.zip"
fullDataPathFile <- paste(dataDir,"/",destFileName,sep="")
download.file(fileUrl,destfile=fullDataPathFile,method="curl")

## manual unzip, working towards function "ddx" which would download data and extract if needed and read it in
## Ideal syntax activity <- ddx(URL, fileName, dataDir="./data", unzip=TRUE|FALSE)
unzip("./data/activity.zip",exdir=dataDir)
activity <- read.csv("./data/activity.csv")


## PREPROCESS DATA / MELT
## INSTALL MELT
install.packages("reshape2")
library(reshape2)

##melt activities
activityMelt <- melt(activity,id=c("date","interval"),measure.vars=c("steps"))




## PART1: Histogram and summary of Daily Steps
##create dcast totals by day
dateTotalActivity <- dcast(activityMelt, date ~ variable, sum)

## giving a few more breaks to improve detail / character of graph
hist(dateTotalActivity$steps,breaks=10,xlab="Total Steps Per Day",main="Histogram of Total Daily Steps")
summary(dateTotalActivity$steps)
mean(dateTotalActivity$steps,na.rm=TRUE)
median(dateTotalActivity$steps,na.rm=TRUE)


## PART2: Time Series Graph of Interval Averages
intervalAverageActivity <- dcast(activityMelt, interval ~ variable, mean,na.rm=TRUE)
plot(intervalAverageActivity$interval,intervalAverageActivity$steps,type="l",xlab="Interval Number",ylab="Average Daily Steps")
intervalAverageActivity[intervalAverageActivity$steps == max(intervalAverageActivity$steps),]
intervalAverageActivity[intervalAverageActivity$steps == max(intervalAverageActivity$steps),]$interval

 
## PART3: Handling NA Values
sum(is.na(activity$steps))
library(plyr)

## PART4: Weekend / Weekday Graph
activity$day = weekdays(as.Date(activity$date))
activity$dayType = ifelse(activity$day == "Sunday" | activity$day == "Saturday","Weekend","Weekday")
activity$dayType <- factor(activity$dayType)
activity$day <- factor(activity$day)
xyplot(steps~interval |dayType, activity,type="l",layout=c(1,2))
xyplot(steps~interval |day, activity,type="l",layout=c(3,3))


