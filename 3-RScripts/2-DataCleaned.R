library(tidyverse)
library(lubridate)
library(janitor)

# Set working directory
setwd("C:/Users/Abdul/Desktop/Google Prject Data")

# =============================================================================
# Reloading data for cleaning
# =============================================================================

daily_activity <- read_csv("1-RawData/dailyActivity_merged.csv")
hourly_calories <- read_csv("1-RawData/hourlyCalories_merged.csv")
hourly_intensities <- read_csv("1-RawData/hourlyIntensities_merged.csv")
hourly_steps <- read_csv("1-RawData/hourlySteps_merged.csv")
minute_sleep <- read_csv("1-RawData/minuteSleep_merged.csv")
weight_log <- read_csv("1-RawData/weightLogInfo_merged.csv")
heartrate <- read_csv("1-RawData/heartrate_seconds_merged.csv")

cat("Data successfully loaded for cleaning\n")

# =============================================================================
# Clean daily activity data (most important file)
# =============================================================================

# Clean column names
daily_activity_clean <- daily_activity %>%
  clean_names() %>%
  mutate(
    activity_date = mdy(activity_date),
    weekday = wday(activity_date, label = TRUE, abbr = FALSE),
    total_active_minutes = very_active_minutes + fairly_active_minutes + lightly_active_minutes,
    calories_per_step = if_else(total_steps > 0, calories / total_steps, 0),
    activity_level = case_when(
      total_steps < 5000 ~ "Low Activity",
      total_steps >= 5000 & total_steps < 10000 ~ "Moderate Activity",
      total_steps >= 10000 & total_steps < 15000 ~ "Good Activity",
      total_steps >= 15000 ~ "High Activity"
    ),
    distance_category = case_when(
      total_distance < 2 ~ "Short Distance",
      total_distance >= 2 & total_distance < 5 ~ "Moderate Distance",
      total_distance >= 5 & total_distance < 8 ~ "Good Distance",
      total_distance >= 8 ~ "Long Distance"
    )
  ) %>%
  filter(
    total_steps >= 0,
    total_distance >= 0,
    calories > 0,
    calories < 6000
  )

cat("Daily activity data cleaned successfully\n")

# =============================================================================
# Clean sleep data
# =============================================================================

sleep_daily <- minute_sleep %>%
  clean_names() %>%
  mutate(
    sleep_date = as_date(mdy_hms(date)),
    sleep_time = hms::as_hms(mdy_hms(date))
  ) %>%
  group_by(id, sleep_date) %>%
  summarise(
    total_sleep_records = n(),
    time_asleep_minutes = sum(value[log_id == 1], na.rm = TRUE),
    time_in_bed_minutes = sum(value[log_id %in% c(1, 2, 3)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    time_asleep_hours = round(time_asleep_minutes / 60, 2),
    time_in_bed_hours = round(time_in_bed_minutes / 60, 2),
    sleep_efficiency = round((time_asleep_minutes / time_in_bed_minutes) * 100, 1),
    sleep_quality = case_when(
      time_asleep_hours < 6 ~ "Short Sleep",
      time_asleep_hours >= 6 & time_asleep_hours < 7 ~ "Less than Recommended",
      time_asleep_hours >= 7 & time_asleep_hours <= 9 ~ "Good Sleep",
      time_asleep_hours > 9 ~ "Oversleeping"
    )
  ) %>%
  filter(
    time_asleep_minutes > 60,
    time_asleep_minutes < 960,
    time_in_bed_minutes > 60
  )

cat("Sleep data cleaned successfully\n")

# =============================================================================
# Clean weight data
# =============================================================================

weight_clean <- weight_log %>%
  clean_names() %>%
  mutate(
    weight_date = as_date(mdy_hms(date)),
    weight_kg = if_else(is.na(weight_kg), weight_pounds * 0.453592, weight_kg),
    bmi_category = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal Weight",
      bmi >= 25 & bmi < 30 ~ "Overweight",
      bmi >= 30 ~ "Obese"
    )
  ) %>%
  filter(
    weight_kg > 30 & weight_kg < 200,
    bmi > 10 & bmi < 60
  ) %>%
  select(-weight_pounds) %>%
  distinct()

cat("Weight data cleaned successfully\n")

# =============================================================================
# Merge hourly datasets
# =============================================================================

hourly_data <- hourly_calories %>%
  clean_names() %>%
  left_join(
    hourly_intensities %>% clean_names(),
    by = c("id", "activity_hour")
  ) %>%
  left_join(
    hourly_steps %>% clean_names(),
    by = c("id", "activity_hour")
  ) %>%
  mutate(
    datetime = mdy_hms(activity_hour),
    date = as_date(datetime),
    hour = hour(datetime),
    weekday = wday(datetime, label = TRUE, abbr = FALSE),
    time_of_day = case_when(
      hour >= 6 & hour < 12 ~ "Morning",
      hour >= 12 & hour < 18 ~ "Afternoon",
      hour >= 18 & hour < 22 ~ "Evening",
      TRUE ~ "Night"
    ),
    calories_per_step_hourly = if_else(step_total > 0, calories / step_total, 0)
  ) %>%
  filter(
    calories > 0 & calories < 1000,
    step_total >= 0 & step_total < 15000
  )

cat("Hourly data merged and cleaned successfully\n")

# =============================================================================
# Create unified dataset for main analysis
# =============================================================================

main_dataset <- daily_activity_clean %>%
  left_join(sleep_daily, by = c("id", "activity_date" = "sleep_date")) %>%
  left_join(
    weight_clean %>% 
      select(id, weight_date, weight_kg, bmi, bmi_category) %>%
      group_by(id) %>%
      slice_max(weight_date, n = 1) %>%
      ungroup() %>%
      select(-weight_date),
    by = "id"
  )

cat("Unified dataset created successfully\n")

# =============================================================================
# Data quality overview after cleaning
# =============================================================================

cat("\nSummary of cleaned data:\n")
cat(paste(rep("=", 50), collapse=""), "\n")

cat("Daily activity records:", nrow(daily_activity_clean), "rows\n")
cat("Unique users:", length(unique(daily_activity_clean$id)), "users\n")

cat("Sleep data records:", nrow(sleep_daily), "rows\n")
cat("Users with sleep data:", length(unique(sleep_daily$id)), "users\n")

cat("Weight data records:", nrow(weight_clean), "rows\n")
cat("Users with weight data:", length(unique(weight_clean$id)), "users\n")

cat("Hourly data records:", nrow(hourly_data), "rows\n")

cat("Unified dataset records:", nrow(main_dataset), "rows\n")

# =============================================================================
# Save cleaned datasets
# =============================================================================

write_csv(daily_activity_clean, "2-CleanedData/daily_activity_clean.csv")
write_csv(sleep_daily, "2-CleanedData/sleep_daily_clean.csv")
write_csv(weight_clean, "2-CleanedData/weight_clean.csv")
write_csv(hourly_data, "2-CleanedData/hourly_data_clean.csv")
write_csv(main_dataset, "2-CleanedData/main_dataset.csv")

cat("\nAll cleaned data saved to Cleaned_Data folder\n")
cat("Next step: Exploratory and statistical analysis\n")

# View sample of main dataset
cat("\nSample of main dataset:\n")
print(head(main_dataset))
