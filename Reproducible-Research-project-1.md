# Project 1

### Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
“quantified self” movement – a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks. But these
data remain under-utilized both because the raw data are hard to obtain
and there is a lack of statistical methods and software for processing
and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

### Loading and preprocessing the data

    activity<- read.csv("/Users/yzhou/Desktop/r/Reproducible Research/activity.csv")

### What is mean total number of steps taken per day?

#### 1.Calculate the total number of steps taken per day.

    activity.totalsteps<- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
    names(activity.totalsteps)<- c("dates", "steps")
    head(activity.totalsteps,10)

    ##         dates steps
    ## 1  2012-10-01     0
    ## 2  2012-10-02   126
    ## 3  2012-10-03 11352
    ## 4  2012-10-04 12116
    ## 5  2012-10-05 13294
    ## 6  2012-10-06 15420
    ## 7  2012-10-07 11015
    ## 8  2012-10-08     0
    ## 9  2012-10-09 12811
    ## 10 2012-10-10  9900

#### 2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

    hist(activity.totalsteps$steps,main = "Total number of steps taken per day",xlab = "Total steps taken per day",col = "blue",ylim = c(0,20),breaks = seq(0,25000, by=2500))

![](Reproducible-Research-project-1_files/figure-markdown_strict/unnamed-chunk-3-1.png)

#### 3.Calculate and report the mean and median of the total number of steps taken per day.

    # mean
    mean(activity.totalsteps$steps)

    ## [1] 9354.23

    # median
    median(activity.totalsteps$steps)

    ## [1] 10395

### What is the average daily activity pattern?

#### 1.Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

    average.daily.activity<- aggregate(activity$steps, by= list(activity$interval), FUN = mean , na.rm = TRUE)
    names(average.daily.activity)<-c("interval", "mean")
    plot(average.daily.activity$interval, average.daily.activity$mean, type = "l", xlab = "Interval", ylab = "Average number of steps", main = "Average number of steps per interval")

![](Reproducible-Research-project-1_files/figure-markdown_strict/unnamed-chunk-5-1.png)

#### 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

    average.daily.activity[which.max(average.daily.activity$mean),]$interval

    ## [1] 835

### Imputing missing values

#### 1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

    sum(is.na(activity$steps))

    ## [1] 2304

#### 2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

    clean.steps<- average.daily.activity$mean[match(activity$interval,average.daily.activity$interval)]

#### 3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

    activity.clean <- transform(activity, steps = ifelse(is.na(activity$steps), yes = clean.steps, no = activity$steps))
    total.clean.steps<- aggregate(steps ~ date, activity.clean, sum)
    names(total.clean.steps)<- c("date", "daily.steps")

#### 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

    hist(total.clean.steps$daily.steps, col = "green", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))

![](Reproducible-Research-project-1_files/figure-markdown_strict/unnamed-chunk-10-1.png)

    # mean
    mean(total.clean.steps$daily.steps)

    ## [1] 10766.19

    # median
    mean(total.clean.steps$daily.steps)

    ## [1] 10766.19

### Are there differences in activity patterns between weekdays and weekends?

#### 1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

    wkday <- function(dat_val) {
      wd <- weekdays(as.Date(dat_val, '%Y-%m-%d'))
      if  (wd == '星期六' || wd == '星期天') {
        x <- 'Weekend'
      } 
      else {
        x <- 'Weekday'
      }
      x
    }

    activity$Day <- as.factor(sapply(activity$date,wkday))

#### 2.Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

    #Graph using ggplot
    library(ggplot2)
    #Aggregate mean of steps on Interval and Day
    activity_1 <- aggregate(steps~interval+Day,activity,mean)
    ggplot(activity_1, aes(x = interval , y = steps, color=`Day`)) + geom_line() + labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap(~`Day` , ncol = 1, nrow=2)

![](Reproducible-Research-project-1_files/figure-markdown_strict/unnamed-chunk-13-1.png)
