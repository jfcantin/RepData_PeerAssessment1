# Reproducible Research: Peer Assessment 1


```r
library(lubridate)
library(data.table)
```

```
## 
## Attaching package: 'data.table'
## 
## The following objects are masked from 'package:lubridate':
## 
##     hour, mday, month, quarter, wday, week, yday, year
```

```r
library(ggplot2)
```



## Loading and preprocessing the data

```r
zipfile <- "activity.zip"
activityFileName <- "activity.csv"

activities <- read.csv(unz(zipfile, activityFileName))
activities$date <- ymd(activities$date)
```


## What is mean total number of steps taken per day?

```r
activities.dt <- as.data.table(activities)

totalStepsPerDay <- activities.dt[!is.na(steps), list(totalSteps = sum(steps)), 
    by = date]
summary(totalStepsPerDay)
```

```
##       date                       totalSteps   
##  Min.   :2012-10-02 00:00:00   Min.   :   41  
##  1st Qu.:2012-10-16 00:00:00   1st Qu.: 8841  
##  Median :2012-10-29 00:00:00   Median :10765  
##  Mean   :2012-10-30 17:12:27   Mean   :10766  
##  3rd Qu.:2012-11-16 00:00:00   3rd Qu.:13294  
##  Max.   :2012-11-29 00:00:00   Max.   :21194
```

```r

# hist(totalStepsPerDay[,totalSteps], breaks='FD')
breaks <- pretty(range(totalStepsPerDay[, totalSteps], na.rm = TRUE), n = nclass.FD(totalStepsPerDay[, 
    totalSteps]), min.n = 1)
g <- ggplot(totalStepsPerDay, aes(x = totalSteps))
g <- g + geom_histogram(breaks = breaks)
g + geom_rug()
```

![plot of chunk steps per day](figure/steps_per_day.png) 

```r

# mean and median total steps per day
totalStepsPerDay[, list(mean = mean(totalSteps), median = median(totalSteps))]
```

```
##     mean median
## 1: 10766  10765
```





## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
