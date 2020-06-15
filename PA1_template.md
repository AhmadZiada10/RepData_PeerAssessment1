---
title: "Reproducible Research Course Project 1"
author: "Ahmad Hossam"
date: "6/15/2020"
output: html_document
keep_md: true
---

# Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

# Assignment
This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

Questions to be answered:

- What is mean total number of steps taken per day?
- What is the average daily activity pattern?
- Imputing missing values
- Are there differences in activity patterns between weekdays and weekends?

# Setting global option to turn warnings off


```r
knitr::opts_chunk$set(warning = FALSE, echo = TRUE)
```

# Loading and preprocessing the data

```r
library(ggplot2)

data<-read.csv("./activity.csv")
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

# 1. What is mean total number of steps taken per day?

```r
totalStepsPerDay<-aggregate(data$steps, list(data$date), sum, na.rm = TRUE)
cols<-c("Date", "Steps")
names(totalStepsPerDay)<-cols

hist(totalStepsPerDay$Steps, xlab = "Steps", main = "Total # of steps taken per day")
```

![plot of chunk 1](figure/1-1.png)

Here's the mean of the total number of steps taken per day

```r
mean(totalStepsPerDay$Steps)
```

```
## [1] 9354.23
```

Here's the median of the total number of steps taken per day

```r
median(totalStepsPerDay$Steps)
```

```
## [1] 10395
```

# 2. What is the average daily activity pattern?

Here's a time series plot of the 5-minute interval and averagne number of steps taken per day

```r
averageStepsPerDay<-aggregate(data$steps, list(data$interval), mean, na.rm = TRUE)
cols<-c("Interval", "Steps")
names(averageStepsPerDay)<-cols

plot(averageStepsPerDay$Interval,averageStepsPerDay$Steps, ylab = "Interval",
     xlab = "Steps", main = "Average daily pattern", type = "l")
```

![plot of chunk 2](figure/2-1.png)

Here's the 5-minute interval cotains maximum number of steps

```r
averageStepsPerDay[which.max(averageStepsPerDay$Steps), ]$Interval
```

```
## [1] 835
```

# 3. Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Here's the total number of missing values in the dataset

```r
sum(is.na(data))
```

```
## [1] 2304
```

Here's the strategy for imputing the missing values which is mean for for the same 5-minute interval

```r
imputedSteps<-averageStepsPerDay$Steps[match(data$interval, averageStepsPerDay$Interval)]
```

Here's the new dataset whcih has it's missing values imputed

```r
imputedData <- transform(data, steps = ifelse(is.na(data$steps), yes = imputedSteps, no = data$steps))
imputedStepsPerDay<-aggregate(imputedData$steps, list(imputedData$date), sum)
cols<-c("Date", "Steps")
names(imputedStepsPerDay)<-cols
```

Here's a histogram of total number of steps per day.

```r
hist(imputedStepsPerDay$Steps, xlab = "Steps", main = "Total # of steps taken per day"
     , breaks = seq(0,25000,by=2500))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

Here's the mean of the total number of steps taken per day

```r
mean(imputedStepsPerDay$Steps)
```

```
## [1] 10766.19
```

Here's the median of the total number of steps taken per day

```r
median(imputedStepsPerDay$Steps)
```

```
## [1] 10766.19
```

# 4. Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
imputedData$date<-as.Date(strptime(imputedData$date, "%Y-%m-%d"))
imputedData$dayType<-sapply(imputedData$date, function(x){
    if(weekdays(x) == "Saturday" || weekdays(x) == "Sunday"){
        y<-"Weekend"
    }else{
        y<-"Weekday"
    }
    y
})
```

Here's a panel plot of the 5-minute interval and the average number of taken steps across all weekdays and weekends

```r
imputedWeekSteps<-aggregate(steps~interval + dayType, imputedData, FUN = mean, na.rm = TRUE)
g<-ggplot(imputedWeekSteps, aes(x = interval, y = steps, color = dayType))
g + geom_line() + facet_wrap(~dayType)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)
