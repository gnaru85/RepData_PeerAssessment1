---
title: "PA1_template"
date: "2024-10-20"
output: 
  html_document: 
    fig_caption: true
    keep_md: true
  md_document: default
---




# 1. Loading and Preprocessing the Data

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the data
activity_data <- read.csv("activity.csv", header = TRUE, sep = ",")

# Check the structure of the data
str(activity_data)

# Convert 'date' column to Date format
activity_data$date <- as.Date(activity_data$date, "%Y-%m-%d")

# Summarize missing values
sum(is.na(activity_data))


# 2. What is the Mean Total Number of Steps Taken Per Day?

# Remove rows with missing values (NA)
activity_data_no_na <- activity_data[!is.na(activity_data$steps), ]

# Calculate total number of steps taken per day
total_steps_per_day <- activity_data_no_na %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))

# Plot histogram of the total number of steps per day
ggplot(total_steps_per_day, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Total Number of Steps per Day", x = "Steps", y = "Frequency")

# Calculate and report the mean and median of the total steps per day
mean_steps <- mean(total_steps_per_day$total_steps)
median_steps <- median(total_steps_per_day$total_steps)

mean_steps
median_steps


# 3. What is the Average Daily Activity Pattern?


# Calculate the average number of steps for each 5-minute interval
avg_steps_per_interval <- activity_data_no_na %>%
  group_by(interval) %>%
  summarise(average_steps = mean(steps))

# Time series plot of the 5-minute interval and average steps
ggplot(avg_steps_per_interval, aes(x = interval, y = average_steps)) +
  geom_line(color = "blue") +
  labs(title = "Average Number of Steps per 5-minute Interval", x = "Interval", y = "Average Steps")

# Find the interval with the maximum number of steps
max_interval <- avg_steps_per_interval[which.max(avg_steps_per_interval$average_steps), ]

max_interval


# 4. Imputing Missing Data

# Calculate the total number of missing values
total_na <- sum(is.na(activity_data$steps))

total_na

# Impute missing values by using the mean for that 5-minute interval
activity_data_imputed <- activity_data

for (i in 1:nrow(activity_data_imputed)) {
  if (is.na(activity_data_imputed$steps[i])) {
    interval_value <- activity_data_imputed$interval[i]
    imputed_value <- avg_steps_per_interval$average_steps[avg_steps_per_interval$interval == interval_value]
    activity_data_imputed$steps[i] <- imputed_value
  }
}

# Check if missing values were filled
sum(is.na(activity_data_imputed$steps))

# Calculate total number of steps per day after imputing missing values
total_steps_imputed <- activity_data_imputed %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))

# Plot histogram of the total number of steps per day (after imputation)
ggplot(total_steps_imputed, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "green", color = "black") +
  labs(title = "Total Steps per Day (After Imputation)", x = "Steps", y = "Frequency")

# Calculate and report mean and median total number of steps per day (after imputation)
mean_steps_imputed <- mean(total_steps_imputed$total_steps)
median_steps_imputed <- median(total_steps_imputed$total_steps)

mean_steps_imputed
median_steps_imputed

# 5. Are There Differences in Activity Patterns Between Weekdays and Weekends?

# Create a factor variable with two levels: "weekday" and "weekend"
activity_data_imputed$day_type <- ifelse(weekdays(activity_data_imputed$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

# Calculate the average steps for each interval and for weekdays vs weekends
avg_steps_day_type <- activity_data_imputed %>%
  group_by(interval, day_type) %>%
  summarise(average_steps = mean(steps))

# Panel plot comparing average steps taken per 5-minute interval across weekdays and weekends
ggplot(avg_steps_day_type, aes(x = interval, y = average_steps, color = day_type)) +
  geom_line() +
  facet_wrap(~ day_type, ncol = 1, nrow = 2) +
  labs(title = "Average Number of Steps per Interval: Weekdays vs Weekends", x = "Interval", y = "Average Steps")

