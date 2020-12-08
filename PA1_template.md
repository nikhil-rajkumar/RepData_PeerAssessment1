---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


# Loading and preprocessing the data

```r
activity <- read.csv("activity.csv", as.is = TRUE)
```

Remove NA in data

```r
good_act <- activity[complete.cases(activity), ]
```

Print out 5 rows

```r
head(good_act)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```


# What is mean total number of steps taken per day?

```r
steps_per_day <- aggregate(steps ~ date, good_act, sum)

hist(steps_per_day$steps, main = "Histogram of total number of steps per day", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Mean of average steps taken is:

```r
round(mean(steps_per_day$steps))
```

```
## [1] 10766
```

## Median of average steps taken is:

```r
median(steps_per_day$steps)
```

```
## [1] 10765
```

# What is the average daily activity pattern?

```r
avg_steps_per_interval <- aggregate(steps ~ interval, good_act, mean)

avg_steps_per_day <- aggregate(steps ~ date, good_act, mean)

plot(avg_steps_per_interval$interval, avg_steps_per_interval$steps, type='l', col=1, main="Average number of steps by Interval", xlab="Time Intervals", ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
interval_idx <- which.max(avg_steps_per_interval$steps)

print (paste("The interval with the highest avg steps is ", avg_steps_per_interval[interval_idx, ]$interval, " and the no of steps for that interval is ", round(avg_steps_per_interval[interval_idx, ]$steps, digits = 1)))
```

```
## [1] "The interval with the highest avg steps is  835  and the no of steps for that interval is  206.2"
```

# Imputing missing values

## Calculating how much missing values exist

```r
missing_value_act <- activity[!complete.cases(activity), ]
nrow(missing_value_act)
```

```
## [1] 2304
```

## Substituing the NA values with the average steps in that interval across all the days

```r
for (i in 1:nrow(activity)) {
    if(is.na(activity$steps[i])) {
        val <- avg_steps_per_interval$steps[which(avg_steps_per_interval$interval == activity$interval[i])]
        activity$steps[i] <- val 
    }
}
steps_per_day_impute <- aggregate(steps ~ date, activity, sum)

hist(steps_per_day_impute$steps, main = "Histogram of total number of steps per day (IMPUTED)", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
## Mean of imputed values

```r
round(mean(steps_per_day_impute$steps))
```

```
## [1] 10766
```

## Median of imputed values

```r
median(steps_per_day_impute$steps)
```

```
## [1] 10766.19
```
We note that the mean and the median has NOT changed because of the imputed values


# Are there differences in activity patterns between weekdays and weekends?

## Function to check if day is weekday or weekend

```r
week_day <- function(date_val) {
    wd <- weekdays(as.Date(date_val, '%Y-%m-%d'))
    if  (!(wd == 'Saturday' || wd == 'Sunday')) {
        x <- 'Weekday'
    } else {
        x <- 'Weekend'
    }
    x
}
```


## Applying function to create new type of variable based on day

```r
activity$day_type <- as.factor(sapply(activity$date, week_day))

library(ggplot2)

steps_per_day_impute <- aggregate(steps ~ interval+day_type, activity, mean)

plt <- ggplot(steps_per_day_impute, aes(interval, steps)) +
    geom_line(stat = "identity", aes(colour = day_type)) +
    theme_gray() +
    facet_grid(day_type ~ ., scales="fixed", space="fixed") +
    labs(x="Interval", y=expression("No of Steps")) +
    ggtitle("No of steps Per Interval by day type")
print(plt)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


We do see some subtle dissimilarities between the average number of steps on weekdays and weekends. For example, it appears that the user started a bit later on weekend mornings and did fewer steps on weekend mornings.

