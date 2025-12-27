# 1. Load the dataset
data <- read.csv("banking_loan_dataset.csv")


str(data)
colnames(data)


data$Loan_Status <- as.factor(data$Loan_Status)


data$Employment_Type <- as.factor(data$Employment_Type)


anova_result <- aov(Loan_Amount ~ Loan_Status * Employment_Type, data = data)


summary(anova_result)