# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv",colClasses = c("integer", "Date", "factor"))
process_activity <- na.omit(activity)
dim(process_activity)
```

```
## [1] 15264     3
```

```r
rownames(process_activity) <- 1:nrow(process_activity)
head(process_activity)
```

```
##   steps       date interval
## 1     0 2012-10-02        0
## 2     0 2012-10-02        5
## 3     0 2012-10-02       10
## 4     0 2012-10-02       15
## 5     0 2012-10-02       20
## 6     0 2012-10-02       25
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1.  Make a histogram of the total number of steps taken each day.


```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
group_day <- group_by(process_activity,date) 
total_step <- summarise(group_day,totalstep=sum(steps))
head(total_step)
```

```
## Source: local data frame [6 x 2]
## 
##         date totalstep
##       (date)     (int)
## 1 2012-10-02       126
## 2 2012-10-03     11352
## 3 2012-10-04     12116
## 4 2012-10-05     13294
## 5 2012-10-06     15420
## 6 2012-10-07     11015
```

```r
g <- ggplot(total_step, aes(date, totalstep))
g + geom_bar(stat="identity") + ggtitle("Total Number of Steps Taken Each Day")    
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)\

2.  Calculate and report the mean and median total number of steps taken per day.


```r
mean(total_step$totalstep)
```

```
## [1] 10766.19
```

```r
median(total_step$totalstep)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avg_step <- aggregate(process_activity$steps,list(interval = as.numeric(as.character(process_activity$interval))), FUN = "mean")
names(avg_step)[2] <- "mean_step"
ggplot(avg_step, aes(interval, mean_step)) + geom_line() + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute Intervals", y = "Average Number of Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)\

2.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avg_step[avg_step$mean_step==max(avg_step$mean_step),]
```

```
##     interval mean_step
## 104      835  206.1698
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity))
```

```
## [1] 2304
```

2.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# fill in missing data with the mean of 5-minute interval
```

3.  Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_new <- activity
for (i in 1:nrow(activity_new)){
  if (is.na(activity_new$steps[i])) {
        activity_new$steps[i] <- avg_step[which(activity_new$interval[i]==avg_step$interval),]$mean_step
    }
}
sum(is.na(activity_new))
```

```
## [1] 0
```

4.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
group_day2 <- group_by(activity_new, date) 
total_step2 <- summarise(group_day2,totalstep=sum(steps))
g2 <- ggplot(total_step2, aes(date, totalstep))
g2 + geom_bar(stat="identity") + ggtitle("Total Number of Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)\

```r
mean(total_step2$totalstep)
```

```
## [1] 10766.19
```

```r
median(total_step2$totalstep)
```

```
## [1] 10766.19
```

```r
mean(total_step2$totalstep)-mean(total_step$totalstep)
```

```
## [1] 0
```

```r
median(total_step2$totalstep)-median(total_step$totalstep)
```

```
## [1] 1.188679
```

After imputing missing data, the mean of total daily number of steps stays constant, the new median statistics become greater.

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.  Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity_new$weekdays <- factor(format(activity_new$date, "%A"))
levels(activity_new$weekdays)
```

```
## [1] "星期二" "星期六" "星期日" "星期三" "星期四" "星期五" "星期一"
```

```r
levels(activity_new$weekdays) <- list(weekday = c("星期一", "星期二","星期三", "星期四", "星期四"),weekend = c("星期六", "星期日"))
# since my system language is Chinese, I have to use some Chinese here, sorry.
levels(activity_new$weekdays)
```

```
## [1] "weekday" "weekend"
```

2.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
avg_step2 <- aggregate(activity_new$steps,
            list(interval = as.numeric(as.character(activity_new$interval)),
                 weekdays = activity_new$weekdays), 
            FUN = "mean")
names(avg_step2)[3] <- "mean_step"
g3 <- qplot(interval, mean_step, data = avg_step2, facets = weekdays ~ .)
g3 + geom_line() + ggtitle("Total Number of Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)\
