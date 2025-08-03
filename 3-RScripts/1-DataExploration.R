library(tidyverse)   
library(lubridate)   
library(skimr)        
library(janitor)      
library(here)         


setwd("C:/Users/Abdul/Desktop/Google Prject Data")  


# Load all data files



data_path <- "1-RawData/"

# Load files
daily_activity <- read_csv(paste0(data_path, "dailyActivity_merged.csv"))
heartrate <- read_csv(paste0(data_path, "heartrate_seconds_merged.csv"))
hourly_calories <- read_csv(paste0(data_path, "hourlyCalories_merged.csv"))
hourly_intensities <- read_csv(paste0(data_path, "hourlyIntensities_merged.csv"))
hourly_steps <- read_csv(paste0(data_path, "hourlySteps_merged.csv"))
minute_calories <- read_csv(paste0(data_path, "minuteCaloriesNarrow_merged.csv"))
minute_intensities <- read_csv(paste0(data_path, "minuteIntensitiesNarrow_merged.csv"))
minute_mets <- read_csv(paste0(data_path, "minuteMETsNarrow_merged.csv"))
minute_sleep <- read_csv(paste0(data_path, "minuteSleep_merged.csv"))
minute_steps <- read_csv(paste0(data_path, "minuteStepsNarrow_merged.csv"))
weight_log <- read_csv(paste0(data_path, "weightLogInfo_merged.csv"))

print(" All files loaded successfully!")


# Initial data overview


# Create a list of all datasets for quick inspection
datasets <- list(
  "daily_activity" = daily_activity,
  "heartrate" = heartrate,
  "hourly_calories" = hourly_calories,
  "hourly_intensities" = hourly_intensities,
  "hourly_steps" = hourly_steps,
  "minute_calories" = minute_calories,
  "minute_intensities" = minute_intensities,
  "minute_mets" = minute_mets,
  "minute_sleep" = minute_sleep,
  "minute_steps" = minute_steps,
  "weight_log" = weight_log
)

# Check size of each file
cat("\n Size of each dataset:\n")
cat(paste(rep("=", 50), collapse=""), "\n")
for(name in names(datasets)) {
  cat(sprintf("%-20s: %d rows × %d columns\n", 
              name, 
              nrow(datasets[[name]]), 
              ncol(datasets[[name]])))
}

# Check number of unique users per file
cat("\n Number of unique users per dataset:\n")
cat(paste(rep("=", 50), collapse=""), "\n")
for(name in names(datasets)) {
  if("Id" %in% names(datasets[[name]])) {
    unique_users <- length(unique(datasets[[name]]$Id))
    cat(sprintf("%-20s: %d users\n", name, unique_users))
  }
}


# Detailed inspection of main files


cat("\n Detailed inspection of main datasets:\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# 1. Daily Activity (Main File)
cat("\n Daily Activity File:\n")
cat(paste(rep("-", 30), collapse=""), "\n")
glimpse(daily_activity)

# Check date range
cat("\nDate range:", 
    min(mdy(daily_activity$ActivityDate)), "to", 
    max(mdy(daily_activity$ActivityDate)), "\n")

# 2. Sleep Data
cat("\n Sleep File:\n")
cat(paste(rep("-", 30), collapse=""), "\n")
glimpse(minute_sleep)

# 3. Weight Data
cat("\n Weight File:\n")
cat(paste(rep("-", 30), collapse=""), "\n")
glimpse(weight_log)


# Initial Data Quality Check


cat("\n Data Quality Check:\n")
cat(paste(rep("=", 40), collapse=""), "\n")

# Check missing values in main datasets
check_missing <- function(df, name) {
  missing_count <- sum(is.na(df))
  total_cells <- nrow(df) * ncol(df)
  missing_percent <- round((missing_count / total_cells) * 100, 2)
  
  cat(sprintf("%-20s: %d missing values (%.2f%%)\n", 
              name, missing_count, missing_percent))
}

check_missing(daily_activity, "daily_activity")
check_missing(minute_sleep, "minute_sleep")
check_missing(weight_log, "weight_log")
check_missing(heartrate, "heartrate")


# Save initial exploration summary


# Create quick summary
exploration_summary <- data.frame(
  Dataset = names(datasets),
  Rows = sapply(datasets, nrow),
  Columns = sapply(datasets, ncol),
  Users = sapply(datasets, function(x) {
    if("Id" %in% names(x)) length(unique(x$Id)) else NA
  })
)

# Save summary
write_csv(exploration_summary, "4-AnalysisResults/01_data_exploration_summary.csv")

cat("\n Exploration summary saved in Analysis_Results folder\n")
cat("\n  Next step: Clean and prepare data for analysis\n")

# Show summary
print(exploration_summary)
