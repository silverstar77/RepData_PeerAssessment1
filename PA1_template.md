Repreducible Research Course - Peer Assessment Project 1
========================================================

Before you load and process the data please make sure that you set your working directory. 

## Loading and preprocessing the data

- Make sure you download the zip file in the preferred working directory.


```r
fileUrl <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./Activity monitoring data.zip") 
list.files()
```

```
##  [1] "Activity monitoring data.zip"                
##  [2] "activity.csv"                                
##  [3] "code.R"                                      
##  [4] "figure"                                      
##  [5] "instructions.pdf"                            
##  [6] "PA1_template.html"                           
##  [7] "PA1_template.md"                             
##  [8] "PA1_template.Rmd"                            
##  [9] "repdata-data-activity.zip"                   
## [10] "Reproducible Research _Project 1 Outline.pdf"
```

- Unzip it and read the csv file into a dataset called "activity". Check the number of observations and the variables' class.


```r
unzip("./Activity monitoring data.zip")
activity <- read.csv("activity.csv")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Convert the date variable in a object of class "Date" and the interval variable into a factor one. Calculate the total number of step taken in each day (ignoring the NA values for now) and save it into a new variable called steps_day.


```r
activity$date <- as.Date(activity$date)
activity$interval <- as.factor(activity$interval)
steps_day <-tapply(activity$steps,activity$date,sum)
```

## What is mean total number of steps taken per day?

Summarize the new variable steps_day (calculated without removing the Na values) and check out the histogram of its values' distribution.


```r
summary(steps_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8840   10800   10800   13300   21200       8
```

```r
hist(steps_day, main="Total Number of Steps Taken per Day", col="light blue", xlab="Steps per Day")
```

![plot of chunk totaldailysteps](figure/totaldailysteps.png) 

## What is the average daily activity pattern?

Calculate the number of steps taken per 5-minute interval averaged across days (making sure the Na values are removed).


```r
steps_interval <-tapply(activity$steps,activity$interval,mean, na.rm=T)
```

Here is the time series plot.

```r
plot(steps_interval, type="l", main="Average # of Steps Taken per a 5-Minute Interval", xlab="5-minute interval identifier", ylab="Average # of steps taken")
```

![plot of chunk timeseries](figure/timeseries.png) 


```r
int <- which.max(steps_interval)
max_time <-attr(int, "names")
attr(int,"names") <- ""
max_steps<-int
```

On average the maximum number of steps occurred in the 104 5-minute interval or within 5 minutes after 835.

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
