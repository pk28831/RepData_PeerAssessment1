---
title: "Reproducible Research: Peer Assessment 1"
output:  html_document
keep_md: yes
---

# Introduction

This report describes the steps needed to analyze the data of one person:  
number of steps in 5-minutes intervals during 2 months in 2012.  
For a more detailed overview see the Readme.md-file in the repo. 
The data came with the repo, so is in the working directory  
The data can be downloaded from: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

# Loading and preprocessing the data


```r
# when data is not dowloaded or unzipped yet:
if(!file.exists("activity.zip")){
        require(downloader)
        require(curl)
        url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
                download.file(url,"activity.zip", method="curl")
}
if (!file.exists("activity.csv")){
        unzip("activity.zip")
}
data<-read.csv("activity.csv",sep=",")
require(lubridate)
data$date<-ymd(data$date)
nrIntervals<-24*60/5 #hours*minutes/interval_length
```

# What is mean total number of steps taken per day?


```r
require(dplyr)
# remove NA
dat<-as.tbl(data)
by_date<- dat %>%
        filter(steps != "NA") %>%
        group_by(date)
totalStepsPerDay<-summarize(by_date,sum(steps))
names(totalStepsPerDay)<-c("date","steps")
meanStepsPerDay=floor(mean(totalStepsPerDay$steps))
medianStepsPerDay=floor(median(totalStepsPerDay$steps))
hist(totalStepsPerDay$steps,xlab="Total steps per day",ylab="Frequency",main="Total steps per day, NA removed")
abline(v=medianStepsPerDay,col="red")
abline(v=meanStepsPerDay, col="green")
text(x=20000,y=20,labels="mean", col="green")
text(x=20000, y=17.5, labels="median", col="red")
```

![plot of chunk calculate mean total steps](figure/calculate mean total steps-1.png) 
  
 
The mean number of steps taken per day is 10766.  
The median number of steps taken per day is 10765.  



## What is the average daily activity pattern?


```r
by_intervals<- dat %>%
        filter(steps != "NA") %>%
        group_by(interval)
averageDailyActivity<-summarize(by_intervals, mean(steps))
names(averageDailyActivity)<-c("interval","steps")
index1<-which(averageDailyActivity$steps==max(averageDailyActivity$steps))
plot(averageDailyActivity$interval, averageDailyActivity$steps, type="l", xlab=" interval", ylab="avg nr steps", main="Average daily steps, NA removed")
```

![plot of chunk average daily activity pattern](figure/average daily activity pattern-1.png) 
  

The 5-minute interval on average across all days in the data set that contains the maximum number of steps taken is 104 of 288, so at 08:35 hrs.


# Imputing missing values  

For determining a strategy for imputing missing variables, it is important to know when these occur.



```r
summary(dat$steps) #report the number of NA in steps
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```

```r
summary(dat$date) # report the number of NA in date
```

```
##         Min.      1st Qu.       Median         Mean      3rd Qu. 
## "2012-10-01" "2012-10-16" "2012-10-31" "2012-10-31" "2012-11-15" 
##         Max. 
## "2012-11-30"
```

```r
summary(dat$interval) # report the number of NA in interval
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0   588.8  1178.0  1178.0  1766.0  2355.0
```

```r
#find the missing values
index_NA<-is.na(data$steps)
total_NA<-sum(index_NA)
daysNoActivity<-length(unique(dat$date))- length(unique(by_date$date))
intervalsNoActivity<-daysNoActivity*nrIntervals
```

So the number of days with missing data is 8, the total number of NA is 2304, which is 8\*24\*60/5.
It turns out that the missing values are caused by the fact that no data was recorded during 8 days. Taking the average steps per day as an input would give a big shift in the intervals, therefore the missing value is replaced by the average of the interval.
The new data set will be created in the chunk below.


```r
dataNoNA<-dat
repl<- rep(averageDailyActivity$steps,61)
dataNoNA$steps[index_NA]<-repl[index_NA]
```

The influence of filling is shown in the following histogram


```r
by_date_NoNA<- dataNoNA %>%
        group_by(date)
totalStepsPerDayNoNA<-summarize(by_date_NoNA,sum(steps))
names(totalStepsPerDayNoNA)<-c("date","steps")
meanStepsPerDayNoNA=floor(mean(totalStepsPerDayNoNA$steps))
medianStepsPerDayNoNA=floor(median(totalStepsPerDayNoNA$steps))
summary(totalStepsPerDayNoNA)
```

```
##       date                steps      
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 9819  
##  Median :2012-10-31   Median :10766  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```

```r
hist(totalStepsPerDayNoNA$steps,main="Total steps per day, NA filled",
     xlab="Toal steps per day",ylab="Frequency")
abline(v=medianStepsPerDayNoNA,col="red")
abline(v=meanStepsPerDayNoNA, col="green")
text(x=20000,y=20,labels="mean", col="green")
text(x=20000, y=17.5, labels="median", col="red")
```

![plot of chunk imputting missing values influence of filling NA](figure/imputting missing values influence of filling NA-1.png) 
  
  
The mean of the total number of steps taken per day is now 10766, while the median is 10766.  
The average does not differ from the one of the original data set. The median is now equal to the average.  
There was only 1 step difference between mean and median in the original data set.  
As the NA were always valid for a whole day, the number of days was increased by imputing averaged interval values for NA's.  
The average did not change, as we corrected with the average steps per day (summation of the average steps for each interval). The median will approach the mean more, as we only add values equal to the mean.


# Are there differences in activity patterns between weekdays and weekends?


```r
require(dplyr)
require(tidyr)
require(ggplot2)
weekdays1<-c("Monday", "Tuesday","Wednesday","Thursday","Friday")
dataNoNA$wDay <-  factor((weekdays(dataNoNA$date) %in% weekdays1),
                         levels=c(FALSE,TRUE), labels=c('weekend', 'weekday'))
dataNoNA$wDay <-as.character(dataNoNA$wDay)
by_intervals_weekday<- dataNoNA %>%
        filter(wDay=="weekday") %>%
        group_by(interval)
by_intervals_weekend<- dataNoNA %>%
        filter(wDay=="weekend") %>%
        group_by(interval)
averageWeekDay<-summarize(by_intervals_weekday, mean(steps))
names(averageWeekDay)<-c("interval","avg nr steps")
averageWeekend<-summarize(by_intervals_weekend, mean(steps))
names(averageWeekend)<-c("interval","avg nr steps")
par(mfrow = c(2, 1))
plot(averageWeekDay,type="l", ylab="avg nr steps", xlab="interval", main="Weekday")
plot(averageWeekend,type="l", ylab="avg nr steps", xlab="interval", main="Weekend")
```

![plot of chunk patterns weekdays vs weekend](figure/patterns weekdays vs weekend-1.png) 

There are differences between weekdays and weekend: in the weekend the number of steps between 10:00 and 16:00 is much higher than on weekdays



