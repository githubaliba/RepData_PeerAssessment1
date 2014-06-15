# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
First, we load the data.  We use parameterize the directory and create the directory if needed.


```r
dataDir <- "./data"
if(!file.exists(dataDir)) {dir.create(dataDir)}
```

Next, we parameterize the URL and filename, and download the file.

```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destFileName <- "activity.zip"
fullDataPathFile <- paste(dataDir,"/",destFileName,sep="")
download.file(fileUrl,destfile=fullDataPathFile,method="curl")
```

Last, we unzip the file to the data directory and load it into the activity data frame.

```r
unzip("./data/activity.zip",exdir=dataDir)
activity <- read.csv("./data/activity.csv")
```

To make things a little easier, we are going to leverage the melt function from the reshape2 library, so we have to install and load that now.


```r
if(!require("reshape2"))
{
  install.packages("reshape2",repos="http://cran.rstudio.com/")
  library(reshape2)
}
```

```
## Loading required package: reshape2
```

Next we "melt" the activity table to allow us to perform some easier operations.


```r
activityMelt <- melt(activity,id=c("date","interval"),measure.vars=c("steps"))
```

At this point, we are ready to begin the analysis.

## What is mean total number of steps taken per day?

Here we are going to leverage the "melted" data to create a new data frame that is the dates and the sum of steps each day.

```r
dateTotalActivity <- dcast(activityMelt, date ~ variable, sum)
```

After that, we can easily generate a histogram of the data using the code below.  Note we generated additional breaks to get a better characterization of the distribution.  We also include a quick summary of the data.

```r
hist(dateTotalActivity$steps,breaks=10,xlab="Total Steps Per Day",main="Histogram of Total Daily Steps")
```

![plot of chunk historgram](figure/historgram.png) 

```r
summary(dateTotalActivity$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8840   10800   10800   13300   21200       8
```

Using the na.rm option, we get an adjusted mean and median here.

```r
mean(dateTotalActivity$steps,na.rm=TRUE)
```

```
## [1] 10766
```

```r
median(dateTotalActivity$steps,na.rm=TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
intervalAverageActivity <- dcast(activityMelt, interval ~ variable, mean,na.rm=TRUE)
plot(intervalAverageActivity$interval,intervalAverageActivity$steps,type="l",xlab="Interval Number",ylab="Average Daily Steps")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

```r
intervalAverageActivity[intervalAverageActivity$steps == max(intervalAverageActivity$steps),]
```

```
##     interval steps
## 104      835 206.2
```

```r
intervalAverageActivity[intervalAverageActivity$steps == max(intervalAverageActivity$steps),]$interval
```

```
## [1] 835
```


## Imputing missing values

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
library(plyr)
```


## Are there differences in activity patterns between weekdays and weekends?


```r
if(!require("lattice"))
{
  install.packages("lattice",repos="http://cran.rstudio.com/")
  library(lattice)
}
```

```
## Loading required package: lattice
```

```r
activity$day = weekdays(as.Date(activity$date))
activity$dayType = ifelse(activity$day == "Sunday" | activity$day == "Saturday","Weekend","Weekday")
activity$dayType <- factor(activity$dayType)
activity$day <- factor(activity$day)
xyplot(steps~interval |dayType, activity,type="l",layout=c(1,2))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-31.png) 

```r
xyplot(steps~interval |day, activity,type="l",layout=c(3,3))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-32.png) 

