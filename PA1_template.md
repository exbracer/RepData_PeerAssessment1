# Reproducible Research: Peer Assessment 1
QIAO Yuchen  


## Loading and preprocessing the data
Firsly ,let's read the data in the csv file and check the data. I read the file into a data.frame and I also need to make the date column into right type. 

```r
# read the data into a data.framework
data <- read.csv(file = "activity.csv", sep = ",", dec = ".", header = TRUE)
# check the date colum into right type
data[, 2] <- as.Date(data[, 2], format = "%Y-%m-%d")
# show the header of the processed data
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(data)
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

## What is mean total number of steps taken per day?
In this part, a histograms is provided to answer the question in the directions. This histogram will show the total number of the steps taken each day. The mean and median of the total number of steps taken per day are also calculated in the R code.


```r
# calculation of the total of steps taken each unique day
uniqueDays <- unique(data$date)
uniqueDays
```

```
##  [1] "2012-10-01" "2012-10-02" "2012-10-03" "2012-10-04" "2012-10-05"
##  [6] "2012-10-06" "2012-10-07" "2012-10-08" "2012-10-09" "2012-10-10"
## [11] "2012-10-11" "2012-10-12" "2012-10-13" "2012-10-14" "2012-10-15"
## [16] "2012-10-16" "2012-10-17" "2012-10-18" "2012-10-19" "2012-10-20"
## [21] "2012-10-21" "2012-10-22" "2012-10-23" "2012-10-24" "2012-10-25"
## [26] "2012-10-26" "2012-10-27" "2012-10-28" "2012-10-29" "2012-10-30"
## [31] "2012-10-31" "2012-11-01" "2012-11-02" "2012-11-03" "2012-11-04"
## [36] "2012-11-05" "2012-11-06" "2012-11-07" "2012-11-08" "2012-11-09"
## [41] "2012-11-10" "2012-11-11" "2012-11-12" "2012-11-13" "2012-11-14"
## [46] "2012-11-15" "2012-11-16" "2012-11-17" "2012-11-18" "2012-11-19"
## [51] "2012-11-20" "2012-11-21" "2012-11-22" "2012-11-23" "2012-11-24"
## [56] "2012-11-25" "2012-11-26" "2012-11-27" "2012-11-28" "2012-11-29"
## [61] "2012-11-30"
```

```r
totalStepsPerDay <- tapply(data$steps, data$date, FUN=sum, na.rm = TRUE)
totalStepsPerDay
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```

```r
# draw the plot of the histogram
with(data, plot(uniqueDays, totalStepsPerDay, type="h", main = "Total numbers of steps taken each day", xlab = "Date from 2012-10-01 to 2012-11-30", ylab = "Total number of steps taken per day", lwd = 4, xaxt = "n"))
axis.Date(1, at = seq(min(data$date), max(data$date), by = "1 day"), format = "%Y-%m")
```

![](PA1_template_files/figure-html/meanNumberSteps-1.png) 

```r
# mean of the total number of steps taken per day
mean(totalStepsPerDay)
```

```
## [1] 9354.23
```

```r
# median of the total number of steps taken per day
median(totalStepsPerDay)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
In this part, a time series plot is drawn in order to show the average number of steps taken per 5-minute interval in each day. The maximum number of steps for each interval is also shown in this part. 

```r
# calculate the average number of steps per interval
stepsPerInterval <- tapply(data$steps, data$interval, FUN = mean, na.rm = TRUE)
uniqueInterval <- unique(data$interval)
# draw the plot for time series of average number of steps per interval
plot(uniqueInterval, stepsPerInterval, type = "l", lwd = 3, main = "Average steps taken per interval across all days", xlab = "Interval", ylab = "Average number of steps taken per interval")
```

![](PA1_template_files/figure-html/activityPattern-1.png) 

```r
# The interval with maximum of steps and the maximum are shown below
stepsPerInterval[which.max(stepsPerInterval)]
```

```
##      835 
## 206.1698
```

## Imputing missing values
Firstly, the total number of missing values in the dataset is calculated. Secondly, the mean of 5-minute interval is chosen for filling in all of the missing values in the dataset. Then a histogram of total number of steps taken each day for the new dataset is drawn and the mean and median total number of steps taken per day are re-calculated. They diff from the estimates from the first part of the assignment.

```r
# calculate the total number of the missing value
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
# calculate the mean of 5-minute interval
meanStepsPerInterval <- aggregate(formula = steps ~ interval, data = data, FUN = mean)
head(meanStepsPerInterval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
new_data <- data
# fill the mean of 5-minute interval in all of the missing values. 
for (i in 1:nrow(new_data)) {
    if (is.na(new_data[i, 1])) {
        new_data[i, 1] <- meanStepsPerInterval[which(meanStepsPerInterval$interval == new_data[i, 3]), 2]
    }
}
# now the total number of the missing value should be 0
sum(is.na(new_data$steps))
```

```
## [1] 0
```

```r
# calculation of the total of steps taken each unique day for the new data set
new_uniqueDays <- unique(new_data$date)
new_uniqueDays
```

```
##  [1] "2012-10-01" "2012-10-02" "2012-10-03" "2012-10-04" "2012-10-05"
##  [6] "2012-10-06" "2012-10-07" "2012-10-08" "2012-10-09" "2012-10-10"
## [11] "2012-10-11" "2012-10-12" "2012-10-13" "2012-10-14" "2012-10-15"
## [16] "2012-10-16" "2012-10-17" "2012-10-18" "2012-10-19" "2012-10-20"
## [21] "2012-10-21" "2012-10-22" "2012-10-23" "2012-10-24" "2012-10-25"
## [26] "2012-10-26" "2012-10-27" "2012-10-28" "2012-10-29" "2012-10-30"
## [31] "2012-10-31" "2012-11-01" "2012-11-02" "2012-11-03" "2012-11-04"
## [36] "2012-11-05" "2012-11-06" "2012-11-07" "2012-11-08" "2012-11-09"
## [41] "2012-11-10" "2012-11-11" "2012-11-12" "2012-11-13" "2012-11-14"
## [46] "2012-11-15" "2012-11-16" "2012-11-17" "2012-11-18" "2012-11-19"
## [51] "2012-11-20" "2012-11-21" "2012-11-22" "2012-11-23" "2012-11-24"
## [56] "2012-11-25" "2012-11-26" "2012-11-27" "2012-11-28" "2012-11-29"
## [61] "2012-11-30"
```

```r
new_totalStepsPerDay <- tapply(new_data$steps, new_data$date, FUN=sum, na.rm = TRUE)
new_totalStepsPerDay
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##   10766.19     126.00   11352.00   12116.00   13294.00   15420.00 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##   11015.00   10766.19   12811.00    9900.00   10304.00   17382.00 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##   12426.00   15098.00   10139.00   15084.00   13452.00   10056.00 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##   11829.00   10395.00    8821.00   13460.00    8918.00    8355.00 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##    2492.00    6778.00   10119.00   11458.00    5018.00    9819.00 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##   15414.00   10766.19   10600.00   10571.00   10766.19   10439.00 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##    8334.00   12883.00    3219.00   10766.19   10766.19   12608.00 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##   10765.00    7336.00   10766.19      41.00    5441.00   14339.00 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##   15110.00    8841.00    4472.00   12787.00   20427.00   21194.00 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##   14478.00   11834.00   11162.00   13646.00   10183.00    7047.00 
## 2012-11-30 
##   10766.19
```

```r
# draw the plot of the histogram
with(new_data, plot(new_uniqueDays, new_totalStepsPerDay, type="h", main = "Total numbers of steps taken each day after fill the missing value", xlab = "Date from 2012-10-01 to 2012-11-30", ylab = "Total number of steps taken per day", lwd = 4, xaxt = "n"))
axis.Date(1, at = seq(min(new_data$date), max(new_data$date), by = "1 day"), format = "%Y-%m")
```

![](PA1_template_files/figure-html/imputingMissingValues-1.png) 

```r
# mean of the total number of steps taken per day
mean(new_totalStepsPerDay)
```

```
## [1] 10766.19
```

```r
# median of the total number of steps taken per day
median(new_totalStepsPerDay)
```

```
## [1] 10766.19
```

```r
# summary of the total number of steps taken per day
summary(new_totalStepsPerDay)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```


## Are there differences in activity patterns between weekdays and weekends?
In this part, I add a new column into the data frame to label the day type. According to the direction, a time series plot will be drawn to show the difference of the number of steps taken in five minute intervals between weekdays and weekends. 

```r
# show the name of the type of the day in English
Sys.setlocale("LC_TIME", "en_CA.UTF-8")
```

```
## [1] "en_CA.UTF-8"
```

```r
# put the data into a new data.frame and initialize the data's day type as weekday
new2_data <- data
new2_data$day_type <- "weekday"
# check if the day type is weekend and make the label
for (i in 1:nrow(new2_data)) {
    if (i == 1) {
        print(weekdays(new2_data[i, 2]))
    }
    if (weekdays(new2_data[i, 2]) == "Saturday") {
        new2_data[i, 4] <- "weekend"
    }
    else if (weekdays(new2_data[i, 2]) == "Sunday") {
        new2_data[i, 4] <- "weekend"
    }
}
```

```
## [1] "Monday"
```

```r
# aggregation by day type and interval 
stepsPerDay2 <- aggregate(formula = steps ~ interval + day_type, data = new2_data, FUN = mean)
# make the plot of the weekday and weekend data in one box
par(mfrow = c(1, 1))
with(stepsPerDay2, plot(steps ~ interval, type = "n", main = "Mean steps taken in weekday vs we. weekend"))
with(stepsPerDay2[stepsPerDay2$day_type=="weekday",], lines(steps~interval, type = "l", col = "blue"))
with(stepsPerDay2[stepsPerDay2$day_type=="weekend",], lines(steps~interval, type = "l", col = "red"))
legend("topright", lty = c(1,1), col = c("blue", "red"), legend = c("weekday", "weekend"), seg.len=4)
```

![](PA1_template_files/figure-html/weekdaysAndWeekends-1.png) 
