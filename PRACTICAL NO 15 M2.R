install.packages("writexl")
library(writexl)

rm(list = ls())

stock <- read.csv("stock_market_dataset.csv")

write.csv(stock, "stock_output.csv", row.names = FALSE)

write_xlsx(stock, "stock_output.xlsx")

stock$Buy <- ifelse(stock$Volume > 1000000, 1, 0)

model1 <- glm(Buy ~ Close, data = stock, family = binomial)

sink("logistic_model_output.txt")
summary(model1)
sink()