
# R Script: Generating Basic Summaries
# Functions: str() and summary()
# Dataset: NBA Data.csv


# 1. Import Dataset
nba_df <- read.csv("NBA Data.csv")
print("--- Data Loaded ---")
print(head(nba_df))


# 2. USING str() (Structure)

print("--- OUTPUT OF str() ---")
str(nba_df)


# 3. USING summary() (Statistical Summary)

print("--- OUTPUT OF summary() [Before Factor Conversion] ---")
summary(nba_df)


# 4. IMPROVING summary() WITH FACTORS

# Convert Team column to factor (Categorical)
nba_df$Team <- as.factor(nba_df$Team)

print("--- OUTPUT OF summary() [After Factor Conversion] ---")
summary(nba_df)


# 5. Accessing Specific Summaries

avg_age <- mean(nba_df$Age, na.rm = TRUE)
max_points <- max(nba_df$Points, na.rm = TRUE)

print(paste("Average Age:", avg_age))
print(paste("Maximum Points Scored:", max_points))
