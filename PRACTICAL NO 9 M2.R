data <- read.csv("hospital_patient_dataset.csv")

# Check the columns to be sure
colnames(data)

# Use 'Disease_Category' instead of 'Diagnosis'
contingency_table <- table(data$Gender, data$Disease_Category)

print(contingency_table)

# Run Chi-Square test
chi_result <- chisq.test(contingency_table)

chi_result