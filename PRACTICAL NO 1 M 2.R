# ============================================================
# Practical 1: Generating Descriptive Statistics
# Dataset: airline_flight_delay_analysis.csv
# ============================================================

# Install and load required packages
if(!require(psych)) install.packages("psych")
library(psych)

# ------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------
df <- read.csv("airline_flight_delay_analysis.csv")

print("--- Dataset Loaded Successfully ---")
# Check column names strictly
print(colnames(df)) 

# ------------------------------------------------------------
# 2. DESCRIPTIVE STATISTICS
# ------------------------------------------------------------

print("--- 1. Descriptive Statistics ---")

# A. Using Base R summary()
# Note: Maine column name 'departure_delay' se 'departure_delay_min' kar diya hai
print("Summary of Departure Delay (in minutes):")
summary(df$departure_delay_min)

print("Summary of Arrival Delay (in minutes):")
summary(df$arrival_delay_min)

# B. Using psych::describe()

# Note: 'flight_distance' data me nahi hai.
# Isliye maine 'scheduled_departure_min' use kiya hai example ke liye.
print("Detailed Description of Scheduled Departure:")
describe(df$scheduled_departure_min)

print("Detailed Description of Departure Delay:")
describe(df$departure_delay_min)
