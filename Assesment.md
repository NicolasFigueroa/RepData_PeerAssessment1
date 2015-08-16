# Reproducible Research: Peer Assessment 1
Nicolas Figueroa  
Sunday, August 16, 2015  


## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())

2. Process/transform the data (if necessary) into a format suitable for your analysis



```r
dataUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zip <- paste(getwd(), "/activity.zip", sep = "")
myFile <- paste(getwd(), "/data/activity.csv", sep = "")
data <- "data"
```



```r
if(!file.exists(data)){
  dir.create(data)
}
```




```r
if(!file.exists(zip)){
  download.file(dataUrl, zip, mode="wb")
}
```



```r
if(!file.exists(myFile)){
    unzip(zip, list = FALSE, overwrite = FALSE, exdir = data)
}
```



```r
activity <- read.table(file = myFile, header = TRUE, sep = ",")
```


## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day


```r
dailySteps = aggregate(steps ~ date, activity, sum, na.rm=T)
```


2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(dailySteps$steps, xlab = "Steps", main = "Histogram of Daily Step Data")
```

![](Assesment_files/figure-html/unnamed-chunk-7-1.png) 




3. Calculate and report the mean and median of the total number of steps taken per day

Mean number of steps per day:


```r
mean(dailySteps$steps, na.rm = T)
```

```
## [1] 10766.19
```

Median number of steps per day:

```r
median(dailySteps$steps, na.rm=T)
```

```
## [1] 10765
```


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?