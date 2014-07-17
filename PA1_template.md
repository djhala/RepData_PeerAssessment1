# Data Analysis
# ==================================

## Loading and preprocessing the data

Before doing anything else, ensure that the data file has been unzipped and is correctly named "activity.csv". Also make sure that working directory is correctly set. We'll begin by reading in the data and changing the column class of the second column to "date". We'll also call the lattice package now; we'll be using it later.


```r
library(lattice)
x <- read.csv("activity.csv")
x[[2]] <- as.Date(x[[2]])
```


## What is mean total number of steps taken per day?

To answer this question, we'll create a new data frame and use a while loop to add rows to it. Each row will consist of a date in column 2 and the total number of steps taken on that date in column 1.


```r
y <- data.frame(Steps = numeric(), Date = character())
y[[2]] <- as.Date(y[[2]])
counter <- x[[2]][1]

while (counter <= max(x[[2]])) {
    DayStep <- subset(x[[1]], x[[2]] == counter)
    SumDayStep <- sum(DayStep)
    y <- rbind(y, c(SumDayStep, counter))
    counter <- counter + 1
}
```


With our new data frame, we have R calculate the mean and median using the obvious functions.


```r
mean(y[[1]], na.rm = T)
```

```
## [1] 10766
```

```r
median(y[[1]], na.rm = T)
```

```
## [1] 10765
```


We'll then create a histogram of the data in the base plotting system.


```r
hist(y[[1]], 11, xlab = "Steps Per Day", main = "")
```

![plot of chunk Rplot](Rplot.png) 


## What is the average daily activity pattern?

To answer this question, we'll create a new data frame with the mean number of steps across all days for a given time in the first column, and the time in the second. We'll accomplish this using a while loop very similar to the one we used previously.


```r
y <- data.frame(Steps = numeric(), Date = character())
y[[2]] <- as.Date(y[[2]])
counter <- x[[3]][1]
while (counter <= max(x[[3]])) {
    DayStep <- subset(x[[1]], x[[3]] == counter)
    MeanDayStep <- mean(DayStep, na.rm = T)
    y <- rbind(y, c(MeanDayStep, counter))
    counter <- counter + 5
}
```


This data set has lots of NA values, because "counter" had 8 values not in x[[3]] for every 12 values in x[[3]]. Before we proceed, we'll removes those values and put better names on the columns.


```r
y <- subset(y, !is.na(y[[1]]))
colnames(y) <- c("Average Steps", "Time")
```


Now we'll plot the data using the base plotting system.


```r
plot(y[[2]], y[[1]], type = "l", xlab = "Time", ylab = "Average Steps")
```

![plot of chunk Rplot01](Rplot01.png) 


Finally, we'll figure out which time has the highest number of average steps. We'll do this by having R print only that subset of the data.


```r
subset(y[[2]], y[[1]] == max(y[[1]]))
```

```
## [1] 835
```


## Imputing missing values

Since we already have the average number of steps for any time interval handy, we'll be using those to substitue for the NA values in the data. To do this, we'll be creating a new data frame just like the old one. A while loop will accomplish this.


```r
nf <- x
counter <- 1
while (counter <= length(nf[[1]])) {
    if (is.na(nf[counter, 1])) {
        nf[counter, 1] <- subset(y[[1]], y[[2]] == nf[counter, 3])
    }
    counter <- counter + 1
}
```


Now we'll run the same while loop we used in the first question to perform the same analysis.


```r
y <- data.frame(Steps = numeric(), Date = character())
y[[2]] <- as.Date(y[[2]])
counter <- nf[[2]][1]

while (counter <= max(nf[[2]])) {
    DayStep <- subset(nf[[1]], nf[[2]] == counter)
    SumDayStep <- sum(DayStep)
    y <- rbind(y, c(SumDayStep, counter))
    counter <- counter + 1
}
```


We'll also use the same code to produce the histogram.


```r
hist(y[[1]], 11, xlab = "Steps Per Day", main = "")
```

![plot of chunk Rplot02](Rplot02.png) 


We can remove "na.rm=T" from our lines to get the mean and median.


```r
mean(y[[1]])
```

```
## [1] 10766
```

```r
median(y[[1]])
```

```
## [1] 10766
```


## Are there differences in activity patterns between weekdays and weekends?

To answer this question, we'll first create a new vector consisting of which day of the week corresponds to each row in the data frame.


```r
p <- weekdays(nf[[2]])
```


Then we'll use a while loop and an if-else statement to create another new vector with weekends and weekdays. We'll then column bind that new vector to the dataframe.


```r
counter <- 1
q <- character()
while (counter <= length(p)) {
    if (p[counter] == "Saturday" | p[counter] == "Sunday") {
        q <- c(q, "Weekend")
    } else {
        q <- c(q, "Weekday")
    }
    counter <- counter + 1
}
nf <- cbind(nf, q)
```


Now we'll create two new data frames based on subsetting the current dataframe by Weekday or Weekend. Then we'll run the same analysis from the daily activity pattern question on each of the new data frames.


```r
WeekEnd <- subset(nf, nf[[4]] == "Weekend")
WeekDay <- subset(nf, nf[[4]] == "Weekday")

WDAVG <- data.frame(Steps = numeric(), Date = character())
WDAVG[[2]] <- as.Date(WDAVG[[2]])
counter <- WeekDay[[3]][1]
while (counter <= max(WeekDay[[3]])) {
    DayStep <- subset(WeekDay[[1]], WeekDay[[3]] == counter)
    MeanDayStep <- mean(DayStep, na.rm = T)
    WDAVG <- rbind(WDAVG, c(MeanDayStep, counter))
    counter <- counter + 5
}
WDAVG <- subset(WDAVG, !is.na(WDAVG[[1]]))

WEAVG <- data.frame(Steps = numeric(), Date = character())
WEAVG[[2]] <- as.Date(WEAVG[[2]])
counter <- WeekEnd[[3]][1]
while (counter <= max(WeekEnd[[3]])) {
    DayStep <- subset(WeekEnd[[1]], WeekEnd[[3]] == counter)
    MeanDayStep <- mean(DayStep, na.rm = T)
    WEAVG <- rbind(WEAVG, c(MeanDayStep, counter))
    counter <- counter + 5
}
WEAVG <- subset(WEAVG, !is.na(WEAVG[[1]]))
```


Now we'll column bind "Weekday" to the weekday data and "Weekend" to the weekend data, change the column names so they all match up, and row bind the data together into one neat data frame.


```r
WDAVG <- cbind(WDAVG, "Weekday")
WEAVG <- cbind(WEAVG, "Weekend")
colnames(WDAVG) <- c("Steps", "Time", "Day")
colnames(WEAVG) <- c("Steps", "Time", "Day")
Total <- rbind(WDAVG, WEAVG)
```


Finally, we'll make a panel plot of the data using the lattice plotting system.


```r
xyplot(Steps ~ Time | Day, data = Total, type = "l")
```

![plot of chunk Rplot03](Rplot03.png) 

