install.packages("dplyr")

library(dplyr)
library(readr)

loans <- read_csv("Loan_approval_data_2025.csv")

head(loans)

high_income_subset <- subset(loans, annual_income > 80000)
cat("Number of high-income customers (annual_income > 80000):", nrow(high_income_subset), "\n")
summary(high_income_subset$annual_income)

good_credit_low_dti_subset <- subset(loans,
                                     credit_score > 700 &
                                       debt_to_income_ratio < 0.3)

cat("Number of customers with good credit and low DTI:", nrow(good_credit_low_dti_subset), "\n")
head(good_credit_low_dti_subset)

risky_customers_subset <- subset(loans,
                                 defaults_on_file == 1 |
                                   delinquencies_last_2yrs > 2)

cat("Number of risky customers (defaults or many delinquencies):", nrow(risky_customers_subset), "\n")
head(risky_customers_subset)

low_interest_filter <- loans |>
  filter(interest_rate < 10)

cat("Number of low-interest loans (interest_rate < 10):", nrow(low_interest_filter), "\n")
summary(low_interest_filter$interest_rate)

large_approved_loans_filter <- loans |>
  filter(loan_status == 1,
         loan_amount > 50000)

cat("Number of large approved loans (loan_amount > 50000):", nrow(large_approved_loans_filter), "\n")
head(large_approved_loans_filter)

home_edu_loans_filter <- loans |>
  filter(loan_intent %in% c("Home Improvement", "Education"))

cat("Number of loans for Home Improvement or Education:", nrow(home_edu_loans_filter), "\n")
table(home_edu_loans_filter$loan_intent)

