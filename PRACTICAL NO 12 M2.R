# Load dataset
df <- read.csv("petrol_diesel_dataset.csv")

# Check structure
str(df)
colnames(df)

# Select only numeric columns
num_data <- df[sapply(df, is.numeric)]

# Remove columns with constant values (to fix correlation error)
num_data <- num_data[, sapply(num_data, function(x) sd(x) != 0)]

# Generate correlation matrix
cor_matrix <- cor(num_data)

# Display correlation matrix
print(cor_matrix)