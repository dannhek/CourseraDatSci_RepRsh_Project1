---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
# Assume we've got the correct working directory.
library(ggplot2)
library(plyr)  #using these later
activity <- read.csv("activity.csv")
summary(activity)  #What are we looking at here?
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```

```r
activity$date <- as.Date(as.character(activity$date))
activity$interval <- as.integer(activity$interval)
```

## What is mean total number of steps taken per day?

```r
# First, let's get the data aggregated by day
activity.byDay <- ddply(activity, .(date), summarize, steps = sum(steps))
# Second,Let's look at what we have
head(activity.byDay)
```

```
##         date steps
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
qplot(steps, data = activity.byDay, geom = "histogram", main = "Steps per Day", 
    xlab = "Steps")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk byDay](figure/byDay.png) 

```r
# Third, let's get the Mean and Median by Day
mean(activity.byDay$steps, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(activity.byDay$steps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
# First, let's get summarize the daily data into something overtime
activity.byInt <- ddply(activity, .(interval), summarize, steps = mean(steps, 
    na.rm = TRUE))
# Second, let's look at what we have
qplot(data = activity.byInt, x = interval, y = steps, group = 1, geom = "line")
```

![plot of chunk byInt](figure/byInt.png) 

```r
# Third, find the maximum average interval
maxIndex <- which.max(activity.byInt$steps)
maxSteps <- activity.byInt$steps[maxIndex]
maxIntvl <- activity.byInt$interval[maxIndex]
as.integer(c(maxIndex, maxSteps, maxIntvl))
```

```
## [1] 104 206 835
```

```r
# So index 104, interval 835 for 206.17 steps, which correponds to the
# time
paste(maxIntvl%/%60, maxIntvl%%60, sep = ":")
```

```
## [1] "13:55"
```



## Imputing missing values

```r
# How many NA Values do we have?
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
# Strategy for filling in NA Value: mean of previous and subsequent
# intervals
rm(activity_sansNA)
```

```
## Warning: object 'activity_sansNA' not found
```

```r
rm(activity_sansNA)
```

```
## Warning: object 'activity_sansNA' not found
```

```r
activity_sansNA <- activity
for (i in 1:nrow(activity_sansNA)) {
    if (is.na(activity_sansNA$steps[i])) {
        preVal <- activity_sansNA$steps[i - 1]
        nxtVal <- activity_sansNA$steps[i + 1]
        newVal <- mean(c(preVal, nxtVal), na.rm = TRUE)
        if (is.nan(newVal)) {
            activity_sansNA$steps[i] <- 0
        } else {
            activity_sansNA$steps[i] <- newVal
        }
        
    }
}
summary(activity_sansNA)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 32.5   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.:  0.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355
```

```r
# First, let's get the data aggregated by day
activity.byDay2 <- ddply(activity_sansNA, .(date), summarize, steps = sum(steps))
# Second,Let's look at what we have
head(activity.byDay2)
```

```
##         date steps
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
qplot(activity.byDay2$steps, geom = "histogram", main = "Steps per Day", xlab = "Steps")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk missing_vals](figure/missing_vals.png) 

```r
# Third, let's get the Mean and Median by Day
mean(activity.byDay2$steps, na.rm = FALSE)
```

```
## [1] 9354
```

```r
median(activity.byDay2$steps, na.rm = FALSE)
```

```
## [1] 10395
```


## Are there differences in activity patterns between weekdays and weekends?

```r
# First, add a column for weekend vs weekday
isweekend <- factor(weekdays(as.Date(activity_sansNA$date)) %in% c("Saturday", 
    "Sunday"), labels = c("Weekend", "Weekday"))
activity_sansNA <- cbind(activity_sansNA, isweekend)
# Second, sum up steps
activity.byInt2 <- aggregate(steps ~ interval + isweekend, data = activity_sansNA, 
    sum)
# And plot it
qplot(data = activity.byInt2, x = interval, y = steps, geom = "line", color = isweekend)
```

![plot of chunk weekends](figure/weekends.png) 

