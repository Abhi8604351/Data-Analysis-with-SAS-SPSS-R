# ==============================================================================
# R Script: Reshaping Data with pivot_longer() and pivot_wider()
# Dataset: Diabetes Prediction Data
# ==============================================================================

library(dplyr)
library(tidyr)

# ==============================================================================
# 1. SETUP: Create and Import Data
# ==============================================================================

# Read data and add a PatientID (Essential for tracking rows during pivots)
df <- read.csv("Diabetes_Prediction.csv", na.strings = c("", "NA")) %>%
  mutate(
    PatientID       = row_number(),              # Row-wise unique ID
    Risk_Probability = diagnosed_diabetes,       # Original probability
    Risk_Percent     = diagnosed_diabetes * 100  # Same value in percent
  ) %>%
  # Selecting fewer columns for clarity (similar to sir's code)
  select(PatientID, id, Risk_Probability, Risk_Percent)

print("--- 1. Original Wide Data (Diabetes) ---")
print(head(df))

# ==============================================================================
# 2. PIVOT_LONGER (Wide to Long)
# ==============================================================================

# Scenario:
# You want to combine 'Risk_Probability' and 'Risk_Percent' into a single column
# called 'Value', with a label column called 'Metric'.
# Useful for: Plotting multiple metrics on one graph (e.g., ggplot2).

long_df <- df %>%
  pivot_longer(
    cols      = c(Risk_Probability, Risk_Percent),  # The columns we want to stack
    names_to  = "Metric",                           # Name for the new column containing header names
    values_to = "Value"                             # Name for the new column containing the numbers
  )

print("--- 2. Long Format (pivot_longer) ---")
# Notice how PatientID 1 now appears TWICE (once for Risk_Probability, once for Risk_Percent)
print(head(long_df, 6))

# ==============================================================================
# 3. PIVOT_WIDER (Long to Wide)
# ==============================================================================

# Scenario:
# You have the 'long_df' created above, and you want to spread it back out
# so that every Metric gets its own column again.
# Useful for: Creating report tables or preparing data for machine learning.

wide_df <- long_df %>%
  pivot_wider(
    names_from  = Metric,   # Which column contains the new header names?
    values_from = Value     # Which column contains the data to fill cells?
  )

print("--- 3. Wide Format (Back to Wide) ---")
print(head(wide_df))

# ==============================================================================
# 4. ADVANCED EXAMPLE (Reshaping for Reporting)
# ==============================================================================

# Here we create risk categories (Low / Medium / High) based on Risk_Probability
# and then spread them into columns.
# Similar idea to sir ke Category A/B/C wale example.

df_risk <- df %>%
  mutate(
    RiskGroup = case_when(
      Risk_Probability < 0.33 ~ "Low",
      Risk_Probability < 0.66 ~ "Medium",
      TRUE                    ~ "High"
    )
  )

risk_pivot <- df_risk %>%
  select(PatientID, RiskGroup, Risk_Percent) %>%
  pivot_wider(
    names_from  = RiskGroup,     # Columns = Low / Medium / High
    values_from = Risk_Percent   # Cell values = Risk percentage
  )

print("--- 4. Risk Group Pivot (Spreading Risk Categories) ---")
print(head(risk_pivot))
