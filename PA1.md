# Reproducible Research: Peer Assessment 1

This is assignment 1 for Reproducable Research. 
This file contains explaination and code required to process activity data as 
directed. 

Load required library:

```r
library(plyr)
```


## Loading and preprocessing the data


```r
a <- read.csv("../data/activity.csv")
af <- a[complete.cases(a),]
```


## What is mean total number of steps taken per day?

The next code block calculates the sums of steps per day and generates a 
histogram of them. 


```r
sums <- aggregate(af[, 1], list(af$date), sum)
hist(sums[,2], main="Steps per day", ylim = c(0,30), xlab="Steps")
```

![](PA1_files/figure-html/unnamed-chunk-3-1.png) 

The mean and median are calculated: 


```r
mean(sums[,2])
```

```
## [1] 10766.19
```

```r
median(sums[,2])
```

```
## [1] 10765
```


## What is the average daily activity pattern?

Plot the daily average steps per time slices: 

```r
slices <- aggregate(af[, 1], list(af$interval), mean)
plot(slices[,1], slices[,2], type="l", xlab = "Time slice", ylab = "Mean steps")
```

![](PA1_files/figure-html/unnamed-chunk-5-1.png) 

Find the time slice with the greatest average number of steps:


```r
slices$Group.1[slices$x == max(slices$x)]
```

```
## [1] 835
```

## Imputing missing values

Count the number of NA values for "Steps:


```r
sum(is.na(a$steps))
```

```
## [1] 2304
```

Replace missing values with the mean value for that interval


```r
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
a2 <- ddply(a, ~ interval, transform, steps = impute.mean(steps))
a2<- a2[order(a2$date, a2$interval), ] 
```

Make a histogram of the total number of steps taken each day


```r
sums2 <- aggregate(a2[, 1], list(a2$date), sum)
hist(sums2[,2], main="Steps per day", ylim = c(0,40), xlab="Steps")
```

![](PA1_files/figure-html/unnamed-chunk-9-1.png) 

The mean and median are calculated: 


```r
mean(sums2[,2])
```

```
## [1] 10766.19
```

```r
median(sums2[,2])
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
