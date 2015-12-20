#Peer Assigment 1 RR
##Load the data

```r
data<-read.csv('activity.csv', stringsAsFactors = FALSE )
```

##Process the data


```r
data$day<-weekdays(as.POSIXlt(data$date, "%Y-%m-%d"))
```

#What is mean total number of steps taken per day?
## Calculate the total number of steps taken per day


```r
steps_day<-aggregate(steps ~ date, data=data, sum, na.rm=T)
head(steps_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
##Make a histogram of the total number of steps taken each day

```r
hist(steps_day$steps, main="Total Steps", xlab="Days")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

##Calculate and report the mean and median of the total number of steps taken per day


```r
mean(steps_day$steps)
```

```
## [1] 10766.19
```

```r
median(steps_day$steps)
```

```
## [1] 10765
```
#What is the average daily activity pattern?
##Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
library(ggplot2)
library(dplyr)

steps_day<-summarise(group_by(data, interval), mean=mean(steps, na.rm=T))
g<-ggplot(steps_day, aes(interval,mean))
g+geom_line()
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
which.max(steps_day$mean)
```

```
## [1] 104
```
#Imputing missing values
##Calculate and report the total number of missing values in the dataset 

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```
##Devise a strategy for filling in all of the missing values in the dataset. 

```r
#The strategy consists in replacing NA by mean
na <- which(is.na(data$steps))
new<- rep(mean(data$steps, na.rm=TRUE), times=length(na))
#Create a new dataset that is equal to the original dataset but with the missing data filled in.
data[na, "steps"] <- new
```
##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
steps_day_2<-summarise(group_by(data, date), total=sum(steps))
hist(steps_day_2$total, main="Total Steps", xlab="Days")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

#Are there differences in activity patterns between weekdays and weekends?
##Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.



```r
data$day<-weekdays(as.POSIXlt(data$date, "%Y-%m-%d"))
data_4<-mutate(data, day_week=ifelse(day=="domingo", "weekend","weekday" ))
# In Spanish: domingo=Sunday
steps_day_2<-summarise(group_by(data_4, interval, day_week), mean=mean(steps))
g<-ggplot(steps_day_2, aes(interval, mean))  + geom_line()
g+facet_grid(day_week ~ .)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 






Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
