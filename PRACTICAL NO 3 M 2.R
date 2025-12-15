# ============================================================
# Practical 3: Cross-Tabulation using table()
# Dataset: uber_driver_trip_analysis.csv
# ============================================================

# Load dataset
df <- read.csv("uber_driver_trip_analysis.csv", stringsAsFactors = FALSE)

# ------------------------------------------------------------
# Select ONLY categorical (character/factor) columns
# ------------------------------------------------------------
cat_cols <- df[sapply(df, function(x) is.character(x) || is.factor(x))]

# Check at least 2 categorical columns exist
if (ncol(cat_cols) < 2) {
  stop("Dataset me 2 categorical columns nahi hain for cross-tabulation")
}

# ------------------------------------------------------------
# Cross-Tabulation
# Rows = First categorical column
# Columns = Second categorical column
# ------------------------------------------------------------

print("--- 3. Cross-Tabulation (Category 1 vs Category 2) ---")

cross_tab <- table(cat_cols[[1]], cat_cols[[2]])
print(cross_tab)
