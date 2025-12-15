# ============================================================
# Practical 3: Cross-Tabulation using table()
# ============================================================

# Load dataset
df <- read.csv("uber_driver_trip_analysis.csv",
               stringsAsFactors = FALSE)

# Select categorical columns
categorical_data <- df[sapply(df, function(x)
  is.character(x) || is.factor(x))]

# Create two-way table
print("--- Cross-Tabulation ---")
cross_tab <- table(categorical_data[[1]],
                   categorical_data[[2]])
print(cross_tab)
