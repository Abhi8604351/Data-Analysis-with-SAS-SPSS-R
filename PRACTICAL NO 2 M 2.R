# ============================================================
# Practical 2: Generating Frequency Tables
# Dataset: before_after_marketing_campaign.csv
# ============================================================

# Install & load required package
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)

# ------------------------------------------------------------
# 1. Load Dataset
# ------------------------------------------------------------
df <- read.csv("before_after_marketing_campaign.csv",
               stringsAsFactors = FALSE)

print("--- Dataset Loaded ---")
print(colnames(df)) # Ye check karein taaki columns dikh jayein

# ------------------------------------------------------------
# 2. Frequency Tables
# ------------------------------------------------------------

print("--- 2. Frequency Tables (Campaign Type Distribution) ---")

# A. Using Base R table()
# Good for: Simple counts

# Correction: 'campaign_phase' -> 'campaign_type'
campaign_counts <- table(df$campaign_type)
print(campaign_counts)

# B. Using dplyr::count()
# Good for: Dataframe output

# Correction: 'campaign_phase' -> 'campaign_type'
campaign_df <- df %>% count(campaign_type)
print(campaign_df)