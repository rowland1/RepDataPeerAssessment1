---
title: "PA1_template"
author: "Dana E Rowland"
date: "May 30, 2018"
output: 
  html_document: 
    fig_caption: yes
    keep_md: yes
---



## Start of project

This markdown file contains the completed work for Course 5 - Reproducable Research, Week 2, peer graded assignment #1. 
For a description of the peer graded assignment, please see: https://class.coursera.org/repdata-002/human_grading/view/courses/972084/assessments/3/submissions 


# Code for reading in the dataset and transforming the data for analysis

The report requires activity.csv to be located in the same folder as PA1_template.Rmd.

To read the data into a dataframe and inspect the structure of the data:


```r
act_data <- read.csv("./activity.csv", header = T)
names(act_data)
```

```
## [1] "steps"    "date"     "interval"
```

```r
str(act_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(act_data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

# Preprocessing steps

Convert date to from integer to date format:


```r
act_data$date <- as.Date(act_data$date , "%Y-%m-%d")
```

Calculate total steps per day:


```r
act_data.day <- aggregate(act_data$steps, list(act_data$date), sum)
names(act_data.day)[1] <- "day"
names(act_data.day)[2] <- "steps"
```

calculate total steps per interval:


```r
act_data.interval <- aggregate(act_data$steps, list(act_data$interval), sum, na.rm=TRUE, na.action=NULL)
names(act_data.interval)[1] <- "interval"
names(act_data.interval)[2] <- "steps"
```

calculate mean steps per interval


```r
act_data.mean.interval <- aggregate(act_data$steps, list(act_data$interval), mean, na.rm=TRUE, na.action=NULL)
names(act_data.mean.interval) [1] <- "interval"
names(act_data.mean.interval) [2] <- "mean.steps"
```

# What is the mean total number of steps taken per day and the related distribution?

Histogram of the total number of steps taken each day


```r
hist(act_data.day$steps, 
     main = "Historgram of Total Steps Taken Each Day",
     xlab = "Total Steps Taken Each Day")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

# Mean and median number of steps taken each day

Mean


```r
mean(act_data.day$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```
Median


```r
median(act_data.day$steps, na.rm = TRUE)
```

```
## [1] 10765
```
# What is the average number of steps taken taken daily?

Time series plot of average steps take daily


```r
plot(act_data.mean.interval$interval, act_data.mean.interval$mean.steps, type = "n", 
     main = "Steps Taken per 5-Minute Interval", 
     xlab = "5-Minute Intervals", 
     ylab = "Average Number of Steps Taken")
lines(act_data.mean.interval$interval, act_data.mean.interval$mean.steps, type = "l")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

# Maximum number of steps

The 5-minute interval that, on average, contains the maximum number of steps


```r
act_data.interval[which.max(act_data.mean.interval$mean.steps),1]
```

```
## [1] 835
```

#Inputing missing values

Code to describe and show a strategy for imputing missing data:

Calculate and report the total number of missing values in the data:


```r
sum(is.na(act_data$steps))
```

```
## [1] 2304
```

Impute missing data and replace NA's with imputed values


```r
act_data.imput <- merge(act_data, act_data.mean.interval, by = "interval", sort = FALSE)
act_data.imput <- act_data.imput[with(act_data.imput, order(date,interval)), ] 
act_data.imput$steps[is.na(act_data.imput$steps)] <- act_data.imput$mean.steps[is.na(act_data.imput$steps)]
act_data.imput$mean.steps <- NULL

head(act_data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
head(act_data.mean.interval)
```

```
##   interval mean.steps
## 1        0  1.7169811
## 2        5  0.3396226
## 3       10  0.1320755
## 4       15  0.1509434
## 5       20  0.0754717
## 6       25  2.0943396
```

```r
head(act_data.imput)
```

```
##     interval     steps       date
## 1          0 1.7169811 2012-10-01
## 63         5 0.3396226 2012-10-01
## 128       10 0.1320755 2012-10-01
## 205       15 0.1509434 2012-10-01
## 264       20 0.0754717 2012-10-01
## 327       25 2.0943396 2012-10-01
```

```r
act_data.imput$steps <- round(act_data.imput$steps, digits = 0)
```
Create a new data set with NA's replaced and rounded


```r
act_data.new <- act_data.imput[, c(2,3,1)]
```

#Check reasonableness of new data set with NA's replaced and rounded

Histogram of the total number of steps taken each day after missing values are imputed


```r
act_data.day.new <- aggregate(act_data.new$steps, list(act_data.new$date), sum)
names(act_data.day.new)[1] <- "day"
names(act_data.day.new)[2] <- "steps"


hist(act_data.day.new$steps, 
     main = "Historgram of Total Steps Taken Each Day (NA's replaced)",
     xlab = "Total Steps Taken Each Day")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

Mean and median of total nubmer of steps taken per day with NA's replaced


```r
mean(act_data.day.new$steps, na.rm = TRUE)
```

```
## [1] 10765.64
```

```r
median(act_data.day.new$steps, na.rm = TRUE)
```

```
## [1] 10762
```

Comparative histogram between old data set with missing values and new data set without missing values


```r
par(mfrow = c(1,2))

hist(act_data.day$steps, 
     main = "Historgram of Total Steps Taken Each Day with NA's",
     xlab = "Total Steps Taken Each Day")

hist(act_data.day.new$steps, 
     main = "Historgram of Total Steps Taken Each Day (NA's replaced)",
     xlab = "Total Steps Taken Each Day")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

#Are there differences in activity patterns between weekdays and weekends?

New factor variable with panel plotcomparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
act_data.new.v2 <- act_data.new
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
act_data.new.v2$weekdays <- factor(format(act_data.new.v2$date, '%A'))
levels(act_data.new.v2$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(act_data.new.v2$weekdays) <- list("weekday" = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "weekend" = c("Saturday", "Sunday"))

head(act_data.new.v2)
```

```
##     steps       date interval weekdays
## 1       2 2012-10-01        0  weekday
## 63      0 2012-10-01        5  weekday
## 128     0 2012-10-01       10  weekday
## 205     0 2012-10-01       15  weekday
## 264     0 2012-10-01       20  weekday
## 327     2 2012-10-01       25  weekday
```

```r
act_data.new.v2.mean.interval <- aggregate(act_data.new.v2$steps, by=list(act_data.new.v2$weekdays, act_data.new.v2$interval), mean, na.rm=TRUE, na.action=NULL)
names(act_data.new.v2.mean.interval)[1] <- "Weekday"
names(act_data.new.v2.mean.interval)[2] <- "Interval"
names(act_data.new.v2.mean.interval)[3] <- "Mean_Steps"

library(lattice)
xyplot(act_data.new.v2.mean.interval$Mean_Steps ~ act_data.new.v2.mean.interval$Interval | act_data.new.v2.mean.interval$Weekday,
          layout=c(1,2), 
          type= "l", 
          xlab = "Interval", 
          ylab = "Number of Days")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-17-1.png" style="display: block; margin: auto;" />

