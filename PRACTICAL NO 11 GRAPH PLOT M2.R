library(ggplot2)

df <- read.csv("gold_market_dataset.csv")

str(df)
colnames(df)

ggplot(df, aes(x = Gold_Price_per_10g)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Gold Price",
       x = "Gold Price per 10g",
       y = "Frequency")

ggplot(df, aes(y = Gold_Price_per_10g)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Box Plot of Gold Price",
       y = "Gold Price per 10g")