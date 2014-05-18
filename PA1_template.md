# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


Load "activity.csv" from the current working directory and convert the date string to a data datatype.



```r
dfAct <- read.csv("activity.csv", stringsAsFactors = FALSE)
dfAct$date <- as.Date(dfAct$date, "%Y-%m-%d")
```



----

## What is mean total number of steps taken per day?

Aggregate the steps by total for each day, then plot a histogram.



```r
dfSumDt <- aggregate(dfAct$steps, by = list(date = dfAct$date), FUN = sum)
colnames(dfSumDt) <- c("date", "steps")
plot(dfSumDt$date, dfSumDt$steps, typ = "h")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 



Calculate the mean and median for the total number of steps taken per day.



```r
avg <- mean(dfSumDt$steps, na.rm = TRUE)
med <- median(dfSumDt$steps, na.rm = TRUE)
```



This is the mean.



```r
avg
```

```
## [1] 10766
```



This is the median.



```r
med
```

```
## [1] 10765
```



----

## What is the average daily activity pattern?


Aggregate the steps by average for each 5 minute interval, then plot a time series.



```r
dfMeanInt <- aggregate(dfAct$steps, by = list(interval = dfAct$interval), FUN = mean, 
    na.rm = TRUE)
colnames(dfMeanInt) <- c("interval", "steps")
plot(dfMeanInt$interval, dfMeanInt$steps, typ = "l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 



The following is the 5 minute interval that contains the maximum number of steps and also the maximum number of steps.



```r
dfMeanInt[which.max(dfMeanInt$steps), ]
```

```
##     interval steps
## 104      835 206.2
```



## Imputing missing values


This is the total number of missing values.



```r
length(dfAct[is.na(dfAct$steps), 1])
```

```
## [1] 2304
```



#### *** The strategy is to fill the missing values with the corresponding average for that particular 5 minute interval.


This is the new dataset with the stated strategy applied.



```r
dfCleanAct <- dfAct
dfCleanAct[is.na(dfCleanAct$steps), "steps"] <- dfMeanInt[match(dfCleanAct[is.na(dfCleanAct$steps), 
    "interval"], dfMeanInt$interval), "steps"]
```



This is the histogram of the total number of steps taken each day.



```r
dfCleanSumDt <- aggregate(dfCleanAct$steps, by = list(date = dfCleanAct$date), 
    FUN = sum)
colnames(dfCleanSumDt) <- c("date", "steps")
plot(dfCleanSumDt$date, dfCleanSumDt$steps, typ = "h")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 



Calculate the mean and median.



```r
avg <- mean(dfCleanSumDt$steps, na.rm = TRUE)
med <- median(dfCleanSumDt$steps, na.rm = TRUE)
```



This is the mean.



```r
avg
```

```
## [1] 10766
```



This is the median.



```r
med
```

```
## [1] 10766
```



The median differs from the first part of the assignment.  Using the strategy above, it has pushed both the sumary values closer to the mean, which is why the median is now the same as the mean.


----

## Are there differences in activity patterns between weekdays and weekends?


This is the new dataset with the day type factor (weekday or weekend)



```r
dfCleanAct$dayType <- ifelse(weekdays(dfCleanAct$date) %in% c("Saturday", "Sunday"), 
    "weekend", "weekday")
```



This plots the 2 panel time series.


```r
tmp1 <- dfCleanAct[dfCleanAct$dayType == "weekend", ]
tmp2 <- dfCleanAct[dfCleanAct$dayType == "weekday", ]

tmp1 <- aggregate(tmp1$steps, by = list(interval = tmp1$interval), FUN = mean, 
    na.rm = TRUE)
tmp1$dayType <- "weekend"
colnames(tmp1) <- c("interval", "steps", "dayType")
tmp2 <- aggregate(tmp2$steps, by = list(interval = tmp2$interval), FUN = mean, 
    na.rm = TRUE)
tmp2$dayType <- "weekday"
colnames(tmp2) <- c("interval", "steps", "dayType")
dfCleanMeanInt <- tmp1
dfCleanMeanInt <- rbind(dfCleanMeanInt, tmp2)


library(lattice)
xyplot(steps ~ interval | dayType, data = dfCleanMeanInt, layout = c(1, 2), 
    type = "l")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 

