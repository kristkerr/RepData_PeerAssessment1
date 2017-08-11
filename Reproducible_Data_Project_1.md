# Reproducible Research Project 1
 
##Loading and preprocessing the data 

```r
library(knitr)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(lattice)
```

1. Load the data.

```r
if(file.exists('activity.csv')){
  unzip('activity.zip')
}
```

```
## Warning in unzip("activity.zip"): error 1 in extracting from zip file
```

```r
activity<-read.csv("activity.csv", header = TRUE)
head(activity)
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

2. Process the data. 

```r
activity$date<-ymd(activity$date)
```

## What is the mean total number of steps taken per day?

1.Calculate the total number of steps per day. 

```r
sumActivity<-aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```

2. Make a histogram of the total number of steps taken each day.

```r
hist(sumActivity$steps, 10, main = "Activity Monitoring Total Steps Per Day", 
     xlab = expression(bold("Total Steps per Day")), 
     ylab = expression(bold(Frequency)),col = "dodgerblue4")
```

![](Reproducible_Data_Project_1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

3. Calculate and report the mean and median total number of steps taken per day.

```r
stepsMean<-mean(sumActivity$steps)
stepsMedian<-median(sumActivity$steps)
```
Mean: 'r stepsMean'
Median: 'r stepsMedian'


##What is the average daily activity pattern?

```r
aggAct<- aggregate(activity$steps ~ activity$interval, activity, mean, na.rm = T)
```

1. Make a time series plot.

```r
plot(aggAct, type = "l", xlab = expression(bold("Interval (minutes)")), 
     ylab = expression(bold("Steps")), main = "Average Daily Activity Pattern")
```

![](Reproducible_Data_Project_1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxInterval<- aggAct[which.max(aggAct$steps), ]
```
Max Interval: 'r maxInterval'

##Inputing missing values

1. Calculate and report the total number of missing values in the dataset. 

```r
rowsNA<-sum(is.na(activity$steps))
```
Total Missing Values: 'r rowsNA'

2. Devise a strategy for filling in all of the missing values in the dataset. 

3. Create a new dataset that is equal to the original dataset, but with the missing data filled in. 

```r
activity2<-activity
NAs<-is.na(activity2$steps)
meanInterval<-with(activity2, tapply(steps, interval, mean, na.rm = T, simplify = TRUE))
activity2$steps[NAs]<-meanInterval[as.character(activity2$interval[NAs])]
```

4. Make a histogram of the total number of steps taken each day...

```r
sumActivity2<-with(activity2, tapply(steps, date, sum))
dSumAct2<-data.frame(Date = names(sumActivity2), Steps = sumActivity2)
hist(dSumAct2$Steps, 10, main = "Activity Monitoring Total Steps Per Day", 
      xlab = expression(bold("Total Steps per Day")), 
      ylab = expression(bold(Frequency)),col = "dodgerblue4")
```

![](Reproducible_Data_Project_1_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

...Calculate and report the mean and median total number of steps taken per day.

```r
stepsMean2<-mean(dSumAct2$Steps)
stepsMedian2<-median(dSumAct2$Steps)
```
Mean: 'r stepsMean2'
Median: 'r stepsMedian2'

##Are there any differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekend" and "weekday".

```r
activity2$day<- weekdays(activity2$date)
activity2$week<- ""
activity2[activity2$day == "Saturday" | activity2$day == "Sunday",]$week<-"weekend"
activity2[!(activity2$day == "Saturday" | activity2$day == "Sunday"),]$week<-"weekday"
```
2. Make a panel plot containing the time series of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday or weekend days(y-axis).


```r
aveStepActivity2<- aggregate(steps ~ interval + week, data = activity2, mean)
xyplot(steps ~ interval|week, data = aveStepActivity2, type = "l", lwd = 2,            layout = c(1,2),
       xlab = "5-minute Interval",
       ylab = "Average Number of Steps",
       main = "Average Number of Steps Taken")
```

![](Reproducible_Data_Project_1_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
