# Code for Project 1 of Coursera Reproducible Research Course
# 9/16/2015

library(ggplot2)
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

# Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# Create a new dataset that is equal to the original dataset but with the missing data filled in.

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

# Are there differences in activity patterns between weekdays and weekends?

# For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


