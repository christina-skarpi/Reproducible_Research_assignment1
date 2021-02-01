---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## 1. Code for reading in the dataset and processing the data

```r
activityDataset <- read.csv(file = 'activity.csv')
head(activityDataset)
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
#### Omitting the NAs from the dataset

```r
activityDatasetProcessed <- na.omit(activityDataset)
head(activityDatasetProcessed)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```
#### Process the date column

```r
activityDatasetProcessed$date <- as.Date.character(activityDatasetProcessed$date, "%Y-%m-%d")
```

## 2. Histogram of the total number of steps taken each day
#### First we need to preprocess the dataset and bring it in a form that it's good to work with.

```r
totalStepsPerDay <- as.data.frame.table(tapply(activityDatasetProcessed$steps, as.Date.character(activityDatasetProcessed$date,  "%Y-%m-%d"), sum))
names(totalStepsPerDay) <- c("date", "steps")
head(totalStepsPerDay)
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
#### Plotting the dataset and results

```r
library(ggplot2)
gsum <- ggplot(data=totalStepsPerDay, aes(x=as.Date(date), y=steps)) + 
            geom_histogram(stat='identity') +
            labs(title = "Total number of steps taken per day", 
                 x = "Date", 
                 y = "Total Steps")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

```r
print(gsum)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## 3. Mean and median number of steps taken each day

```r
meanStepsPerDay <- mean(totalStepsPerDay$steps)
meanStepsPerDay
```

```
## [1] 10766.19
```

```r
medianStepsPerDay <- median(totalStepsPerDay$steps)
medianStepsPerDay
```

```
## [1] 10765
```

## 4. Time series plot of the average number of steps taken
#### First we aggregate the steps using the mean function by interval.

```r
aggdata <-aggregate(activityDatasetProcessed$steps, 
                    by = list(activityDatasetProcessed$interval),
                    FUN = mean)
names(aggdata) <- c("interval", "AggSteps")
head(aggdata)
```

```
##   interval  AggSteps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```
#### Plotting the dataset and results 

```r
gmean <- ggplot(data=aggdata, aes(x=interval, y=AggSteps)) + 
            geom_line() + 
            labs(title = "Time series plot of the avg num. of steps", 
                 x = "Interval", 
                 y = "Avg Steps")

print(gmean)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

## 5. The 5-minute interval that, on average, contains the maximum number of steps

```r
subset(aggdata, aggdata$AggSteps == max(aggdata$AggSteps))
```

```
##     interval AggSteps
## 104      835 206.1698
```

## 6. Code to describe and show a strategy for imputing missing data

#### I have processed my dataset at the beginning of this file to remove the NAs and produce a clean dataset which is called activityDatasetProcessed. The raw dataset is called activityDataset and it's the initial dataset that before any preprocessing step, so it includes  the NA (missing values)

#### Let's see how many missing values has the initial dataset.

```r
sum(is.na(activityDataset))
```

```
## [1] 2304
```
#### Now, let's see how many missing values has the processed dataset which I am using for my analysis. 

```r
sum(is.na(activityDatasetProcessed))
```

```
## [1] 0
```
#### One strategy for imputing data is to replace the NAs with the mean/median. 

```r
dataImputingNAs <- data.frame(sapply(
        activityDataset,
        function(x) ifelse(is.na(x),
            mean(x, na.rm = TRUE),
            x)))
head(dataImputingNAs)
```

```
##              steps       date interval
## 1 37.3825995807128 2012-10-01        0
## 2 37.3825995807128 2012-10-01        5
## 3 37.3825995807128 2012-10-01       10
## 4 37.3825995807128 2012-10-01       15
## 5 37.3825995807128 2012-10-01       20
## 6 37.3825995807128 2012-10-01       25
```
#### If we want to check now how many missing values we have, we can see that it's 0.

```r
sum(is.na(dataImputingNAs))
```

```
## [1] 0
```

## 7. Histogram of the total number of steps taken each day after missing values are imputed
#### We have imputed the NA values from the original dataset in the previous step and storing the new data frame with the name dataImputingNAs.

```r
# Transform the date column into a "Date" column format and the steps into integer.
dataImputingNAs$date <- as.Date(dataImputingNAs$date)
dataImputingNAs$steps <- as.integer(dataImputingNAs$steps)
```
#### Aggregate the steps for each date and use sum as aggregation function.

```r
aggdataImputedNAs <-aggregate(dataImputingNAs$steps, 
                    by = list(dataImputingNAs$date),
                    FUN = sum)
names(aggdataImputedNAs) <- c("date", "AggSteps")
head(aggdataImputedNAs)
```

```
##         date AggSteps
## 1 2012-10-01    10656
## 2 2012-10-02      126
## 3 2012-10-03    11352
## 4 2012-10-04    12116
## 5 2012-10-05    13294
## 6 2012-10-06    15420
```
### Plotting dataset and results

```r
gsumImputed <- ggplot(data=aggdataImputedNAs, aes(x=date, y=AggSteps)) + 
            geom_histogram(stat='identity') + 
            labs(title = "Total number of steps taken each day(with imputed values)", 
                 x = "date", 
                 y = "Avg Steps")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

```r
print(gsumImputed)
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
#### Creating this classification of whether a date is weekend or weekday.

```r
dataImputingNAs$weekdays <- weekdays(dataImputingNAs$date)
dataImputingNAs$weekend <- ifelse(dataImputingNAs$weekday=="Saturday" | dataImputingNAs$weekday=="Sunday", "Weekend", "Weekday")
```
#### Aggregate the steps for weekend or weekday and use mean as aggregation function.

```r
aggdataAVGImputedNAs <-aggregate(dataImputingNAs$steps, 
                    by = list(dataImputingNAs$weekend, dataImputingNAs$interval),
                    FUN = sum)
names(aggdataAVGImputedNAs) <- c("classification", "interval", "steps")
aggdataAVGImputedNAs$interval <- as.integer(aggdataAVGImputedNAs$interval)
```
#### Plot the AVG number of steps per 5 mins intervals across weekdays/weekends. 

```r
gAVGweekdayORweekends <- ggplot(data=aggdataAVGImputedNAs, 
                                aes(x=interval, 
                                    y=steps, 
                                    color = classification)) + 
                                    facet_grid(classification ~.) + 
                                    geom_line() + 
                                    labs( x = "intervals", 
                                     y = "Avg Steps",
                                     title = "AVG steps taken per 5-minute interval across weekdays and weekends")
print(gAVGweekdayORweekends)
```

![](PA1_template_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

## 9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
#### This is being accomblised as presented above. 
