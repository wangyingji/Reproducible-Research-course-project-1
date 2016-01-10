
# 1.Loading and preprocessing the data
llibrary(knitr)
library(dplyr)
library(ggplot2)
opts_chunk$set(echo = TRUE)
# load data
data_row <- read.csv('activity.csv')
# remove NA in data
data <- data_row[ with (data_row, { !(is.na(steps)) } ), ]
# print out first 20 rows
head(data,20)



## 2.What is mean total number of steps taken per day?
# first aggregate data by date
by_day <- group_by(data, date)
steps_by_day <- summarise(by_day, total = sum(steps))
steps_by_day
# plot histogram for quick visual inspection of data, 25 bins here
hist(steps_by_day$total, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")
summary(steps_by_day)


## 3. What is the average daily activity pattern?
# preprocessing data for plot
steps_by_interval <- aggregate(steps ~ interval, data, mean)

# create a time series plot 
plot(steps_by_interval$interval, steps_by_interval$steps, type='l', 
     main="Average number of steps over all days", xlab="Interval", 
     ylab="Average number of steps")
# find row with max of steps
max_steps_row <- which.max(steps_by_interval$steps)

# find interval with this max
steps_by_interval[max_steps_row, ]



## 4. Imputing missing values
sum(is.na(data_row))
#I picked the strategy of replacing NA¡¯s with the mean for that 5-minute interval.

data_imputed <- data_row
for (i in 1:nrow(data_imputed)) {
        if (is.na(data_imputed$steps[i])) {
                interval_value <- data_imputed$interval[i]
                steps_value <- steps_by_interval[
                        steps_by_interval$interval == interval_value,]
                data_imputed$steps[i] <- steps_value$steps
        }
}
#I¡¯ve created new data set data_no_na which equals to data_row but without NA¡¯s. All NA¡¯s are replaced with mean of 5-minute interval.

# calculate  total number of steps taken each day
df_imputed_steps_by_day <- aggregate(steps ~ date, data_imputed, sum)
head(df_imputed_steps_by_day)
#hist
hist(df_imputed_steps_by_day$steps, main="Histogram of total number of steps per day (imputed)", 
     xlab="Total number of steps in a day")
# get mean and median of imputed data
mean(df_imputed_steps_by_day$steps)
median(df_imputed_steps_by_day$steps)
# get mean and median of data without NA's
mean(steps_by_day$total)
median(steps_by_day$total)
#Mean values stays the same but therer is slight difference in meadian value.


## 5. Are there differences in activity patterns between weekdays and weekends?
data_imputed['type_of_day'] <- weekdays(as.Date(data_imputed$date))
data_imputed$type_of_day[data_imputed$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
data_imputed$type_of_day[data_imputed$type_of_day != "weekend"] <- "weekday"
# convert type_of_day from character to factor
data_imputed$type_of_day <- as.factor(data_imputed$type_of_day)

# calculate average steps by interval across all days
df_imputed_steps_by_interval <- aggregate(steps ~ interval + type_of_day, data_imputed, mean)

# creat a plot
qplot(interval, 
      steps, 
      data = df_imputed_steps_by_interval, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
        facet_wrap(~ type_of_day, ncol = 1)