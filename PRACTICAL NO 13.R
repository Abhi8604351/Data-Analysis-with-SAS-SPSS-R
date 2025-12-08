

library(dplyr)


# 1. SETUP: Import Dataset with Intentional Duplicates

meets_df <- read.csv("meets.csv")

print("--- 1. Original Dataset ---")
print(meets_df)

# 2. IDENTIFYING DUPLICATES (Before removing them)

duplicates_report <- meets_df %>%
  group_by(Name, Phone) %>%
  count() %>%
  filter(n > 1)

print("--- 2. Duplicate Rows Identified ---")
print(duplicates_report)

# 3. HANDLING EXACT DUPLICATES (Remove fully same rows)

clean_exact <- meets_df %>%
  distinct()

print("--- 3. After Removing Exact Duplicates ---")
print(clean_exact)


# 4. HANDLING DUPLICATES BASED ON SPECIFIC COLUMN

# Keep unique Phone (one record per person)
unique_persons <- meets_df %>%
  distinct(Name, Phone, .keep_all = TRUE)

print("--- 4. Unique Persons Only (Based on Name + Phone) ---")
print(unique_persons)
