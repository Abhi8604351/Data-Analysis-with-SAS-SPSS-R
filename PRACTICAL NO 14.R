
library(lubridate)
library(dplyr)


# 1. SETUP: Import CSV Data

yt_df <- read.csv("youtube_tech_channels_20251120_133753.csv")

print("--- Original Data (Preview) ---")
print(head(yt_df))


# 2. PARSE AND EXTRACT DATE COMPONENTS

processed_data <- yt_df %>%
  mutate(
    Actual_Date = ymd(JoinedDate),        # Convert text → Date
    Year_Num = year(Actual_Date),        # Extract Year
    Month_Num = month(Actual_Date),      # Month number 1-12
    Month_Name = month(Actual_Date, label = TRUE), # Month name
    Day_Num = day(Actual_Date),          # Day of the month
    Weekday_Num = wday(Actual_Date),     # Weekday number (1-7)
    Weekday_Name = wday(Actual_Date, label = TRUE, abbr = FALSE), # Full Weekday
    Quarter = quarter(Actual_Date),      # Quarter 1-4
    Day_of_Year = yday(Actual_Date)      # Day of year 1-366
  )

print("--- Data with Extracted Date Components ---")
print(head(processed_data))


# 3. Extracting Components from System Date (NOW)

current_time <- now()

print("--- Current Time Extraction ---")
print(paste("Current Year:", year(current_time)))
print(paste("Current Hour:", hour(current_time)))
print(paste("Current Minute:", minute(current_time)))
