# Bike-Share Capstone Project
# Author: Ayush Adhikari
# Tool: R (RStudio)
# Purpose: Analyze bike-share data to identify user behavior and provide business insights

# Load libraries
library(tidyverse)
library(lubridate)

# Load dataset
bike_data <- read_csv("bike_data_raw.csv")

# Data Cleaning
bike_data_clean <- bike_data %>%
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at),
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins")),
    day_of_week = wday(started_at, label = TRUE),
    month = month(started_at, label = TRUE)
  ) %>%
  filter(ride_length > 0)  # Remove negative durations

# Remove NA values
bike_data_clean <- drop_na(bike_data_clean)

# Group by user type and summarize ride duration
summary_stats <- bike_data_clean %>%
  group_by(member_casual) %>%
  summarize(
    avg_ride_length = mean(ride_length),
    median_ride_length = median(ride_length),
    max_ride_length = max(ride_length),
    total_rides = n()
  )

# Visualize ride count by day of week
ggplot(bike_data_clean, aes(x = day_of_week, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "Ride Count by Day of Week", x = "Day", y = "Ride Count")

# Save cleaned data
write_csv(bike_data_clean, "cleaned_bike_data.csv")

# Export summary statistics
write_csv(summary_stats, "summary_stats.csv")
