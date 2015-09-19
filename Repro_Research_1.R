# Code for Project 1 of Coursera Reproducible Research Course
# 9/16/2015

library(ggplot2)
library(gridExtra)
library(plyr) # Has to be before dplyr
library(dplyr)

# 1. Load the data
activity <- read.csv("activity.csv")
message(paste0("#Records in activity: "), nrow(activity))

# Process/transform the data (if necessary) into a format suitable for your analysis
# Filter out the NAs
activity_clean <- activity[complete.cases(activity),]
message(paste0("#Records in activity_clean: "), nrow(activity_clean))


# What is mean total number of steps taken per day?
# Calculate the total number of steps taken per day
daily_steps <- ddply(activity_clean, .(date), summarize, sum_steps=sum(steps))
# Make a histogram of the total number of steps taken each day
qplot(sum_steps, data=daily_steps, geom="histogram")

# Calculate and report the mean and median of the total number of steps taken per day
mean_steps <- mean(daily_steps$sum_steps)
median_steps <- median(daily_steps$sum_steps)
message(paste0("daily average number of steps:", round(mean_steps,2)))
message(paste0("median of steps taken per day:", round(median_steps,2)))


# What is the average daily activity pattern?
interval_steps <- ddply(activity_clean, .(interval), summarize, sum_steps=sum(steps))
g <- ggplot(interval_steps,aes(interval, sum_steps))
g <- g + geom_point()
g

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
nb_days <- nrow(daily_steps)  # Number of days for which we have observations
# compute the average number of steps per interval
av_steps <- mutate(interval_steps, sum_steps = sum_steps/nb_days)
colnames(av_steps) <- c("interval", "avrg_steps")
g <- ggplot(av_steps, aes(interval, avrg_steps))
g <- g+geom_line(size=1,colour="blue")
g


# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
av_steps_ordered <- av_steps[with(av_steps, order(-avrg_steps)),]
message(paste0("Interval with maximum number of steps: ", av_steps_ordered[1,"interval"], " -> average #steps: ",
	round(av_steps_ordered[1,"avrg_steps"],2) ))

# Imputing missing values

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
activity_NA <- filter(activity, is.na(steps))
na_rows <- nrow(activity_NA)
message(paste0("#rows with missing data: "), na_rows)

# Devise a strategy for filling in all of the missing values in the dataset.
# Replace NA values with the average values obtained from records with non-missing values: av_steps
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
# Round the averages 
# activity_full <- mutate(activity, steps = ifelse(is.na(steps), round(av_steps[av_steps$interval == interval, "avrg_steps"],0), steps))
activity_full <- mutate(activity, steps = ifelse(is.na(steps), round(av_steps[match(interval,av_steps$interval), "avrg_steps"],0), steps))
if(any(is.na(activity_full))) stop ("still some NAs")


# Make a histogram of the total number of steps taken each day
daily_steps_full <- ddply(activity_full, .(date), summarize, sum_steps=sum(steps))
qplot(sum_steps, data=daily_steps_full, geom="histogram")

# Calculate and report the mean and median total number of steps taken per day.
mean_steps_full <- mean(daily_steps_full$sum_steps)
median_steps_full <- median(daily_steps_full$sum_steps)

message(paste0("daily average number of steps - after NA replacement:", round(mean_steps_full,2)))
message(paste0("median of steps taken per day  - after NA replacement:", round(median_steps_full,2)))
# Do these values differ from the estimates from the first part of the assignment?
# What is the impact of imputing missing data on the estimates of the total daily number of steps?

# Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here.
# Use the dataset with the filled-in missing values for this part.
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend”
# indicating whether a given date is a weekday or weekend day.
day_vs_end <- function(date_str) {
	wkday <- weekdays(as.Date(date_str))
	ifelse(wkday == "Saturday" | wkday == "Sunday", "weekend", "weekday")
}
activity_final <- mutate(activity_full, day_vs_end = day_vs_end(date))
activity_final <- mutate(activity_final, day_vs_end = as.factor(day_vs_end))
activity_weekday <- activity_final[activity_final$day_vs_end == "weekday", c("interval", "steps")]
activity_weekend <- activity_final[activity_final$day_vs_end == "weekend", c("interval", "steps")]
# interval_steps_final <- ddply(activity_final, .(interval), summarize, sum_steps=sum(steps))


# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
# See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
nb_wkdy <- nrow(activity_weekday)
nb_wknd <- nrow(activity_weekend)
interval_steps_weekday <- ddply(activity_weekday, .(interval), summarize, av_steps=sum(steps)/nb_wkdy)
interval_steps_weekend <- ddply(activity_weekend, .(interval), summarize, av_steps=sum(steps)/nb_wknd)
message(paste0("Average number of steps per 5-min on weekdays: ", round(sum(interval_steps_weekday$av_steps),2)))
message(paste0("Average number of steps  per 5-min on weekend days: ", round(sum(interval_steps_weekend$av_steps),2)))

g <- ggplot(interval_steps_weekday,aes(interval, av_steps))
g <- g+geom_line(size=1,colour="blue") + ggtitle("Weekdays") + ylim(0,1.0)
q <- ggplot(interval_steps_weekend,aes(interval, av_steps))
q <- q+geom_line(size=1,colour="red") + ggtitle("Weekend") + ylim(0,1.0)
library(gridExtra)
grid.arrange(g, q, nrow=2)



