df <- read.csv("professional_hr_employee_dataset.csv")


str(df)
colnames(df)


t_test_two <- t.test(Monthly_Salary ~ Attrition_Status, data = df)

print(t_test_two)