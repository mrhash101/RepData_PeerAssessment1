# Reproducible Research: Peer Assessment 1
========================================================


## Loading and preprocessing the data
Load data using the read.csv() command and store it in the data variable. Don't forget to set the correct parameters


```r
data <- read.csv("activity.csv", header = TRUE, na.strings = NA)
# nrow(data)
```


After loading the activity.csv file in to a dataframe, we need to convert the date column from factor to a date using as.Date() function. This command does not produce anything to print on the screen. 


```r
data$date <- as.Date(data$date)
```


## What is mean total number of steps taken per day?

Use the aggregate function to find the sum of values listed in front of every date. 

```r
s <- aggregate(steps ~ date, data = data, FUN = function(x) c(sum = sum(x)))  ##calculating the sum of steps on one date
```


Using the values stored in variable s to create histogram for total number of steps on each day 


```r
plot(s$date, s$steps, type = "h", xlab = "Date", ylab = "Steps", main = "Total steps vs each day")  #histogram of total number of steps/day
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


Calculating the mean of steps on each date.


```r
m_steps <- aggregate(steps ~ date, data = data, FUN = function(x) c(mean = mean(x)))  # mean/day
```


Mean of all the steps taken at all days with NA values not taken in to account in the calculation


```r

m <- mean(s$steps, na.rm = TRUE)
m
```

```
## [1] 10766
```


Calculating the median of steps on each date.


```r
med_steps <- aggregate(steps ~ date, data = data, FUN = function(x) c(median = median(x)))  # median/day
```


Median of all the steps taken at all days with NA values not taken in to account in the calculation


```r

med <- median(s$steps, na.rm = TRUE)
med
```

```
## [1] 10765
```


## What is the average daily activity pattern?

Calculating the means of steps by groups of intervals on each day


```r
require(plyr)
```

```
## Loading required package: plyr
```

```r
x <- ddply(data, "interval", grp.mean.values = mean(steps))
```



Average daily activity


```r
plot(x$steps ~ x$interval, type = "l", xlab = "Interval", ylab = "Average daily Steps", 
    main = "Activity Pattern (Daily)")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


A close look at the plot shows that the maximum activity is going on at '555' or '600'th interval 

## Imputing missing values

The total number of rows with NA in this dataset can be calculated as follows:


```r
nrow(data[!complete.cases(data), ])
```

```
## [1] 2304
```


A good strategy to fill in the NAs would be to substitute using statistics of centrality. We will need the DMwR package for this purpose. Install and source the DMwR package in the workspace, and then call the centralImputation function. The code to do this will be as follows:


```r
data1 <- data  # making an extra copy for later comparison
require(DMwR)
```

```
## Loading required package: DMwR
## Loading required package: lattice
## Loading required package: grid
## KernSmooth 2.23 loaded
## Copyright M. P. Wand 1997-2009
## 
## Attaching package: 'DMwR'
## 
## The following object is masked from 'package:plyr':
## 
##     join
```

```r
data1 <- centralImputation(data1)
```


histogram of the total number of steps taken each day 


```r
s1 <- aggregate(steps ~ date, data = data1, FUN = function(x) c(sum = sum(x)))
plot(s1$date, s1$steps, type = "h", xlab = "Date", ylab = "Steps", main = "Total steps vs each day (No NAs)")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 


Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps


```r
# m_steps1<- aggregate(steps~date, data=data1, FUN=function(x)
# c(mean=mean(x))) ## mean/day after filling the data
m1 <- mean(s1$steps)  #mean of all the values
m1
```

```
## [1] 9354
```


**median** total number of steps taken per day


```r
med1 <- median(s1$steps)
med1
```

```
## [1] 10395
```


## Are there differences in activity patterns between weekdays and weekends?

Adding Names to the date by using the weekday() function as suggested. The names of days (Sunday, Monday, .... ) will be stored in a vector called day_names. A sample of what how many different names are in there will be printed by the unique() function


```r
day_names <- weekdays(data1$date, abbreviate = FALSE)
unique(day_names)
```

```
## [1] "Monday"    "Tuesday"   "Wednesday" "Thursday"  "Friday"    "Saturday" 
## [7] "Sunday"
```


Since the requirement is not the day names but the terms 'weekend' and 'weekday'. We need to convert the day_names into these two terms. Use the gsub function repeatedly for every day. Sample will be printed using the unique() function


```r
day_names1 <- gsub("Saturday", "Weekend", day_names)
day_names1 <- gsub("Sunday", "Weekend", day_names1)
day_names1 <- gsub("Monday", "Weekday", day_names1)
day_names1 <- gsub("Tuesday", "Weekday", day_names1)
day_names1 <- gsub("Wednesday", "Weekday", day_names1)
day_names1 <- gsub("Thursday", "Weekday", day_names1)
day_names1 <- gsub("Friday", "Weekday", day_names1)
unique(day_names1)
```

```
## [1] "Weekday" "Weekend"
```


Combining the day_names1 back into the main data frame using cbind. The order of values has not been changed. Just to check how it looks, we can use the head() function



```r
data1 <- cbind(data1, day_names1)
head(data1)
```

```
##   steps       date interval day_names1
## 1     0 2012-10-01        0    Weekday
## 2     0 2012-10-01        5    Weekday
## 3     0 2012-10-01       10    Weekday
## 4     0 2012-10-01       15    Weekday
## 5     0 2012-10-01       20    Weekday
## 6     0 2012-10-01       25    Weekday
```


Re-calculating the average number of steps by intervals on each day


```r
x1 <- ddply(data1, "interval", grp.mean.values = mean(steps))
```


Finally, creating the panel plots to compare the weekend activity vs weekday activity


```r
library(ggplot2)
p <- qplot(interval, steps, data = x1, geom = "smooth", facets = . ~ day_names1, 
    main = "Activity of Weekdays")
print(p)
```

```
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20.png) 

