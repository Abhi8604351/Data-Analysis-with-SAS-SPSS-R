library(ggplot2)
library(readr)

rm(list = ls())

weather <- read.csv("usa_weather.csv")

colnames(weather) <- c("Date", "City", "State", "Temp", "Rainfall", "Humidity", "Wind", "WeatherType", "Pressure")

model_lm <- lm(Rainfall ~ Temp + Humidity + Wind + Pressure, data = weather)

summary(model_lm)

weather$Predicted_Rain <- predict(model_lm, newdata = weather)

plot(
  weather$Rainfall,
  weather$Predicted_Rain,
  main = "Actual vs Predicted Rainfall",
  xlab = "Actual Rainfall (mm)",
  ylab = "Predicted Rainfall (mm)",
  pch = 16,
  col = "blue"
)

abline(lm(Predicted_Rain ~ Rainfall, data = weather), col = "red", lwd = 2)