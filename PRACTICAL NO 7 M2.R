data <- read.csv("uber_ride_dataset.csv")

# Check the column names
colnames(data)

# Use Ride_Type
data$Ride_Type <- as.factor(data$Ride_Type)


anova_result <- aov(Fare_Amount ~ Ride_Type, data = data)

summary(anova_result)