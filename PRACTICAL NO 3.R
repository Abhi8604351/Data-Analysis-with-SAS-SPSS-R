install.packages(c("readr", "psych"))

library(readr)    # For efficient data reading
library(psych)    # For descriptive statistics

my_data <- read.csv("BostonHousing.csv")

# View the first few rows
head(my_data)

# View the last few rows
tail(my_data)

# Get the dimensions (rows and columns)
dim(my_data)

# Get the dimensions (rows and columns)
cat("Dimensions (Rows, Columns): ", dim(my_data), "\n")

# Get the structure (variable types and number of observations)
str(my_data)


# See a summary of the dataset
summary(my_data)

# Get the column names
names(my_data)
cat("Column Names: ", names(my_data), "\n")

# Use the 'psych' package for more detailed descriptive statistics
# 'describe()' provides: n, mean, sd, median, trimmed mean, mad, min, max, range, skew, kurtosis, and se.
describe(my_data)