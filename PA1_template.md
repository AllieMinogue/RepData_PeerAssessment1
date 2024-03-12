---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: true
  pdf_document: default
---
Loading knitr & rmarkdown library

```r
library(knitr)
library(rmarkdown)
```
## A. Loading and preprocessing the data


```r
activity <- read.csv(unz("./activity.zip","activity.csv"),na.strings = "NA")
```

## B. What is mean total number of steps taken per day?

### 1. Calculate the total number of steps taken per day

```r
library(dplyr)
```

```r
# Load the data into an object named "day.activity" & calculate the total number of steps taken per day (column total.by.day)
day.activity <- activity %>% group_by(date) %>% summarise(total.by.day = sum(steps),mean.by.day = mean(steps))
print(day.activity)
```

```
## # A tibble: 61 × 3
##    date       total.by.day mean.by.day
##    <chr>             <int>       <dbl>
##  1 2012-10-01           NA      NA    
##  2 2012-10-02          126       0.438
##  3 2012-10-03        11352      39.4  
##  4 2012-10-04        12116      42.1  
##  5 2012-10-05        13294      46.2  
##  6 2012-10-06        15420      53.5  
##  7 2012-10-07        11015      38.2  
##  8 2012-10-08           NA      NA    
##  9 2012-10-09        12811      44.5  
## 10 2012-10-10         9900      34.4  
## # ℹ 51 more rows
```

### 2. Make a histogram of the total number of steps taken each day

```r
hist(day.activity$total.by.day)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### 3. Calculate and report the mean and median of the total number of steps taken per day

```r
# Calculate the mean (B.3.1):
B.3.1 <- mean(day.activity$total.by.day,na.rm = TRUE)
print(B.3.1)
```

```
## [1] 10766.19
```

```r
# Calculate the median (B.3.2):
B.3.2 <- median(day.activity$total.by.day,na.rm = TRUE)
print(B.3.2)
```

```
## [1] 10765
```

## C. What is the average daily activity pattern?

### 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
# create the data frame with average steps per time interval
activity.pattern <- activity %>% group_by(interval) %>% summarise(mean.by.interval = mean(steps,na.rm = TRUE))

# create the required plot
plot(activity.pattern$interval,activity.pattern$mean.by.interval,type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max.interval <- activity.pattern[which.max(activity.pattern$mean.by.interval),1]
print(max.interval)
```

```
## # A tibble: 1 × 1
##   interval
##      <int>
## 1      835
```

## D. Inputing missing values
### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
### 2. Devise a strategy for filling in all of the missing values in the dataset

```r
merged.activity <- merge(activity,activity.pattern,by = "interval",all.x = TRUE)
merged.activity$steps[is.na(merged.activity$steps)] <- merged.activity$mean.by.interval[is.na(merged.activity$steps)]
```

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
merged.activity <- merged.activity[order(merged.activity$date),1:3]
# test for NA value
sum(is.na(merged.activity$steps))
```

```
## [1] 0
```
### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
Make the histogram:

```r
merged.day.activity <- merged.activity %>% group_by(date) %>% summarise(total.by.day = sum(steps))
hist(merged.day.activity$total.by.day)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
Calculate and report the mean (D.4.1) and median (D.4.2) total number of steps taken per day

```r
D.4.1 <- mean(merged.day.activity$total.by.day)
print(D.4.1)
```

```
## [1] 10766.19
```

```r
D.4.2 <- median(merged.day.activity$total.by.day)
print(D.4.2)
```

```
## [1] 10766.19
```
Do these values differ from the estimates from the first part of the assignment? Let's check the difference:

```r
D.4.1 - B.3.1
```

```
## [1] 0
```

```r
D.4.2 - B.3.2
```

```
## [1] 1.188679
```
What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
# Create a column named "difference" in merged.day.activity to measure the difference of total daily number of steps compared to original data
merged.day.activity$difference <- merged.day.activity$total.by.day - day.activity$total.by.day
print(merged.day.activity$difference)
```

```
##  [1] NA  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [26]  0  0  0  0  0  0 NA  0  0 NA  0  0  0  0 NA NA  0  0  0 NA  0  0  0  0  0
## [51]  0  0  0  0  0  0  0  0  0  0 NA
```
Observation here is:
- For dates with available data (no. of steps), there is no impact, since the data are available for all the intervals within such dates
- For dates without available data (NA in no. of steps), the revised data provide such date at estimated level

## E. Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
merged.activity$date <- as.Date(merged.activity$date)
merged.activity$day <- weekdays(merged.activity$date)
merged.activity$wd.wk <- ifelse(merged.activity$day %in% c("Saturday","Sunday"),"weekend","weekday")
```

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
wd.activity <- merged.activity %>% filter(wd.wk == "weekday") %>% group_by(interval) %>% summarise(mean.by.interval = mean(steps))
wk.activity <- merged.activity %>% filter(wd.wk == "weekend") %>% group_by(interval) %>% summarise(mean.by.interval = mean(steps))
par(mfcol = c(2:1),mar = c(4,4,2,2))
plot(wd.activity$interval,wd.activity$mean.by.interval,type = "l",main = "weekday",xlab = "interval",ylab = "Number of steps")
plot(wk.activity$interval,wk.activity$mean.by.interval,type = "l",main = "weekend",xlab = "interval",ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

