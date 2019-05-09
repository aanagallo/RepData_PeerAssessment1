---
title: "Reproducible Research: Peer Assessment 1"
author: "Anthony Oliver A. Nagallo"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data
Load the data you need

```r
activity <- read.csv("activity.csv")
```
Trasnform data in the right format

```r
activity$date<-as.Date(as.character(activity$date))
```

## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day
If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
steps <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(steps) <- c("Date", "Steps")
hist(steps$Steps, main = "Total steps per day", xlab = "Steps per day", col = "blue",ylim = c(0,20), breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day

Mean


```r
mean(steps$Steps) #Mean of the Total Number of Steps
```

```
## [1] 9354.23
```
Median


```r
median(steps$Steps) #Median of the Total Number of Steps
```

```
## [1] 10395
```
## What is the average daily activity pattern?

Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
ave_steps <- aggregate(activity$steps, by = list(activity$interval), FUN = mean, na.rm = TRUE)
names(ave_steps) <- c("interval", "mean")
plot(ave_steps$interval, ave_steps$mean, type = "l", lwd =2, ylab = "Mean Steps", xlab = "Interval", main = "Average steps per Interval" )
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
ave_steps[which.max(ave_steps$mean),]$interval
```

```
## [1] 835
```

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
TotalNAs <- sum(is.na(activity$steps))
TotalNAs
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
impute <- activity
impute$steps[which(is.na(impute$steps))] <- mean(impute$steps, na.rm = TRUE)
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
imputesteps <- with(impute, aggregate(steps, by = list(date), FUN = sum))
names(imputesteps) <- c("Date", "Steps")
hist(imputesteps$Steps, main = "Total steps per day", xlab = "Steps per day", col = "blue",ylim = c(0,20), breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

New Mean 


```r
mean(imputesteps$Steps)
```

```
## [1] 10766.19
```

New Median


```r
median(imputesteps$Steps)
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activity$date <- as.Date(activity$date)
weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activity$date <- factor((weekdays(activity$date)%in% weekdays),levels = c(FALSE, TRUE), labels = c('weekend','weekday'))
```

Make a panel plot containing a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
bydate <- aggregate(steps~interval + date, activity, mean, na.rm = TRUE)
library(ggplot2)
ggplot(bydate, aes(x = interval, y = steps, color = date)) +
        geom_line()+
        labs(title = "Average daily steps by type of date", x = "Interval", y = "Average Steps") + facet_wrap(~date, ncol =1, nrow = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
