---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---





This is the first assignment for Coursera *Reproducible research* course.

This assignment uses data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data can be downloaded [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip).

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken

## Loading and preprocessing the data
First, the data is read from the URL provided. The data for the variable date is converted into class Date.


```r
unzip("activity.zip")
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)
```


## What is mean total number of steps taken per day?

The total number of steps taken per day is calculated and stored in a data frame.


```r
require(dplyr)
steps_per_day.0 <- data %>% group_by(date) %>% summarize(steps=sum(steps))
steps_per_day.0 <- as.data.frame(steps_per_day.0)
```
The histograph showing the total numbers of steps taken per day is shown below.


```r
require(ggplot2)
ggplot(steps_per_day.0, aes(x=steps)) + 
  geom_histogram(fill="green") +
  labs(title="Total Number of Steps per Day", x="", y="Count")
```

![](PA1_template_files/figure-html/number_of_steps_per_day-1.png)<!-- -->

Now mean and median of the total number of steps taken per day are calculated.


```r
steps_mean.0 <- mean(steps_per_day.0$steps, na.rm = TRUE)
```


```r
steps_median.0 <- median(steps_per_day.0$steps, na.rm = TRUE)
```

The mean of the total number of steps taken per day is 1.0766189\times 10^{4} and the median is 10765.


## What is the average daily activity pattern?

To analyze the average daily activity, a time series plot or the 5-minute interval and the average number of steps taken is shown below.

First, the mean of steps taken per interval is calculated and stored in a data frame.


```r
time_series <- data %>% group_by(interval) %>% summarize(int_mean=mean(steps,na.rm = TRUE))
time_series <- as.data.frame(time_series)
```

Then, a time series plot is drawn below.


```r
ggplot(time_series, aes(time_series$interval, time_series$int_mean)) + 
    geom_line() +
    labs(title="Average Steps per Interval", x="Interval", y="Average number of steps")
```

![](PA1_template_files/figure-html/average_steps_per_interval-1.png)<!-- -->

Now we calculate the 5-minute interval on average across all days that contains the maximum number of steps.


```r
max_interval <- time_series[which.max(time_series$int_mean),]$interval
```

The 5-minute interval that contains the maximum number of steps the 835th.


## Imputing missing values

There are a number of days/intervals where there are missing values in the data. The presence of missing days may introduce bias into some calculations or summaries of the data set. The total number of missing values in the data set is calculated below.


```r
data_na <- sum(is.na(data))
```

The data set has 2304 missing values out of 17568 observations.

In order to reduce the impact of NA values, the missing values are filled with the average number of steps for that interval across all days, calculated in time_series.


```r
m <- inner_join(data, time_series, by = c("interval", "interval"))
n <- is.na(m$steps)
m[n,]$steps = m[n,]$int_mean
filled_data <- arrange(select(m, steps, date, interval), date)
```

We redraw the histogram with the total number of steps per day and calculate the mean and median.


```r
steps_per_day.1 <- filled_data %>% group_by(date) %>% summarize(steps=sum(steps))
steps_per_day.1 <- as.data.frame(steps_per_day.1)
ggplot(steps_per_day.0, aes(x=steps)) + 
  geom_histogram(fill="red") +
  labs(title="Total Number of Steps per Day", x="", y="Count")
```

![](PA1_template_files/figure-html/number_of_steps_per_day_filled-1.png)<!-- -->

```r
steps_mean.1 <- mean(steps_per_day.1$steps)
steps_median.1 <- median(steps_per_day.1$steps)
```

The new mean of steps per day is 1.0766189\times 10^{4} (cf. 1.0766189\times 10^{4}) and the new median is 1.0766189\times 10^{4} (cf. 1.0766189\times 10^{4}). Inputting averages do not change dramatically either the graph or the distribution mean and median (11000ish).


## Are there differences in activity patterns between weekdays and weekends?

Whether the day was a weekday or a weekend day may cause a difference in the number of steps. To check this hypothesis, a new factor variable with two levels ("weekday" and "weekend") is created in the data set.


```r
w_data <- mutate(filled_data, day = as.character(weekdays(date)))
w_data$day[w_data$day == "Saturday" | w_data$day == "Sunday"] <- "weekend"
w_data$day[!w_data$day == "weekend"] <- "weekday"
```

Then, the mean of steps taken per type of day is calculated and stored in a data frame.


```r
w_series <- w_data %>% group_by(interval,day) %>% summarize(w_mean=mean(steps))
w_series <- as.data.frame(w_series)
```

Now we draw a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.



```r
ggplot(w_series, aes(w_series$interval, w_series$w_mean)) + 
  geom_line(colour="blue") + facet_grid(day ~ .) + 
  theme(strip.background = element_rect(fill="#FFCC66")) +
  labs(title="Average Steps per Interval", x="Interval", y="Number of steps")
```

![](PA1_template_files/figure-html/average_steps_per_interval_across_weekdays-1.png)<!-- -->

