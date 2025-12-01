# R Script (Adapted for INFINITY_DATA_SET.csv)
# 10. Creating new variables using transformations and calculations
# Dataset: INFINITY_DATA_SET.csv (uploaded)

library(dplyr)
library(tidyr)   # for replace_na

# ------------------------------------------------------------------
# 1. IMPORT
# ------------------------------------------------------------------
df <- read.csv("INFINITY_DATA_SET.csv", na.strings = c("", "NA"), stringsAsFactors = FALSE)

# Quick look (optional)
cat("--- Dimensions of original data ---\n")
print(dim(df))
cat("--- Column names ---\n")
print(names(df))

# ------------------------------------------------------------------
# 2. PRE-CLEANING: ensure numeric columns are numeric and fill NAs
# ------------------------------------------------------------------
# Columns in your INFINITY DATA SET used here:
# Price, DiscountPercent, Rating, StockQty

df_clean <- df %>%
  mutate(
    # ensure numeric types (if they came as character)
    Price = as.numeric(Price),
    DiscountPercent = as.numeric(DiscountPercent),
    Rating = as.numeric(Rating),
    StockQty = as.integer(StockQty),
    
    # replace NA with sensible defaults for calculations
    Price = replace_na(Price, 0),
    DiscountPercent = replace_na(DiscountPercent, 0),
    Rating = replace_na(Rating, 0),
    StockQty = replace_na(StockQty, 0)
  )

# ------------------------------------------------------------------
# 3. METHOD A: ARITHMETIC CALCULATIONS (Final price)
# ------------------------------------------------------------------
# Recompute Discount_Amount and Final_Price to be sure (will overwrite if existed)
df_calc <- df_clean %>%
  mutate(
    Discount_Amount = round(Price * (DiscountPercent / 100), 2),
    Final_Price = round(Price - Discount_Amount, 2)
  )

# Show sample
cat("--- Sample (Price, DiscountPercent, Discount_Amount, Final_Price) ---\n")
print(head(df_calc %>% select(Price, DiscountPercent, Discount_Amount, Final_Price), 6))

# ------------------------------------------------------------------
# 4. METHOD B: CONDITIONAL LOGIC (labels)
# ------------------------------------------------------------------
# Quality_Label based on Rating, Price_Category from Final_Price threshold
df_logic <- df_calc %>%
  mutate(
    Quality_Label = ifelse(Rating > 4.0, "Top Rated", "Average"),
    # Adjust threshold if you want; using 4000 as in your earlier code
    Price_Category = ifelse(Final_Price > 4000, "Premium", "Budget")
  )

cat("--- Sample (Rating, Quality_Label, Final_Price, Price_Category) ---\n")
print(head(df_logic %>% select(Rating, Quality_Label, Final_Price, Price_Category), 6))

# ------------------------------------------------------------------
# 5. METHOD C: TEXT TRANSFORMATION (Product_Summary)
# ------------------------------------------------------------------
# Using Category, StockQty and Price to create a short summary string
df_text <- df_logic %>%
  mutate(
    Product_Summary = paste0(Category, " - ", Subcategory, ": ", StockQty, " pcs, Rs. ", Price)
  )

cat("--- Sample Product_Summary ---\n")
print(head(df_text$Product_Summary, 6))

# ------------------------------------------------------------------
# 6. ALL TOGETHER (final combined dataset)
# ------------------------------------------------------------------
final_dataset <- df_text %>%
  mutate(
    Is_High_Value = ifelse(Final_Price > 2000, TRUE, FALSE),
    Status_Report = paste0("Rating: ", round(Rating,1), " / Disc: ", DiscountPercent, "%")
  )

cat("--- Final combined sample ---\n")
print(head(final_dataset %>% select(ProductID, SKU, ProductName, Price, DiscountPercent, Final_Price,
                                    StockQty, Rating, Quality_Label, Price_Category, Product_Summary,
                                    Is_High_Value, Status_Report), 6))

# ------------------------------------------------------------------
# 7. SAVE modified dataset
# ------------------------------------------------------------------
write.csv(final_dataset, "INFINITY_DATA_SET_modified.csv", row.names = FALSE)
cat("Saved modified dataset as: INFINITY_DATA_SET_modified.csv\n")
