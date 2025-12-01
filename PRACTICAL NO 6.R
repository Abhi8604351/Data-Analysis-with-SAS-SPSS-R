# Install and load the dplyr package
install.packages("dplyr")
library(dplyr)

# Read the big CSV files
jan <- read.csv("amazon_january_sales_big.csv")
feb <- read.csv("amazon_february_sales_big.csv")
newp <- read.csv("amazon_new_products_big.csv")

# Merge January and February data using ID and Product
merged <- full_join(jan, feb, by = c("ID", "Product"))

# Replace missing values and calculate total sales
merged <- merged %>%
  mutate(
    Jan_Sales = ifelse(is.na(Jan_Sales), 0, Jan_Sales),
    Feb_Sales = ifelse(is.na(Feb_Sales), 0, Feb_Sales),
    Total_Sales = Jan_Sales + Feb_Sales
  )

# Append new products to January data
final <- bind_rows(jan, newp)

# Save the merged and final data into new CSV files
write.csv(merged, "merged_output.csv", row.names = FALSE)
write.csv(final, "final_output.csv", row.names = FALSE)

