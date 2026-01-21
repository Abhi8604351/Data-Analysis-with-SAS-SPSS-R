library(readr)

rm(list = ls())

df <- read.csv("real_estate_housing.csv")

colnames(df) <- c("Date", "City", "State", "Type", "Beds", "Baths", "Size", "Price", "Year", "Status")

df <- df[1:50, ]

df$Sold <- ifelse(df$Status == "Sold", 1, 0)

model1 <- glm(Sold ~ Size, family = binomial, data = df)

summary(model1)

plot(
  df$Size,
  df$Sold,
  main = "Logistic Regression: Size vs Sold (Subset)",
  xlab = "House Size (sqft)",
  ylab = "Sold Probability",
  pch = 19,
  col = ifelse(df$Sold == 1, "darkgreen", "red") # Green for Sold, Red for Available
)

x_val <- seq(min(df$Size), max(df$Size), length.out = 100)
pred_prob <- predict(model1, newdata = data.frame(Size = x_val), type = "response")

lines(x_val, pred_prob, col = "blue", lwd = 2)