# ============================================================================
# 9. Performing text manipulation using str_sub(), str_split()
# Dataset: Flipkart_Data_2025.csv  (Imported)
# ============================================================================

# Load libraries
library(stringr)
library(tidyr)
library(dplyr)

# ============================================================================
# 1. IMPORT DATASET
# ============================================================================
flipkart <- read.csv("Flipkart_Data_2025.csv")

print("--- Original Dataset (First 5 rows) ---")
print(head(flipkart, 5))

# ============================================================================
# 2. USING str_sub()  (Substring)
# Extract specific parts from SKU
# ============================================================================

# A: First 4 characters → Category Code
flipkart$Category_Code <- str_sub(flipkart$SKU, 1, 4)

# B: Last 4 characters → Year
flipkart$Year <- str_sub(flipkart$SKU, -4, -1)

print("--- After str_sub() ---")
print(head(flipkart %>% select(SKU, Category_Code, Year), 5))

# ============================================================================
# 3. USING str_split() (Split string)
# Split 'Description' like 'Electronics - Smart TV'
# ============================================================================

# Method A: Basic split (list format)
split_list <- str_split(flipkart$Description, " - ")
print("--- Basic Split (First row) ---")
print(split_list[[1]])

# Method B: Simplify to matrix for easy column creation
split_matrix <- str_split(flipkart$Description, " - ", simplify = TRUE)

flipkart$Main_Cat <- split_matrix[, 1]
flipkart$Sub_Cat  <- split_matrix[, 2]

print("--- After str_split() (Manual) ---")
print(head(flipkart %>% select(Description, Main_Cat, Sub_Cat), 5))

# ============================================================================
# 4. BONUS: Using separate()
# Automatically splits SKU into 3 columns
# ============================================================================
tidy_flipkart <- flipkart %>%
  separate(SKU, into = c("Dept", "Prod_ID", "Mfg_Year"), sep = "-")

print("--- Using separate() ---")
print(head(tidy_flipkart %>% select(Dept, Prod_ID, Mfg_Year), 5))
