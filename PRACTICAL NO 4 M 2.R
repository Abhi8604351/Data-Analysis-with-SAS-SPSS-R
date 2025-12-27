df <- read.csv("ecommerce_sales_dataset.csv")

str(df)
colnames(df)

# one sample t-test
t_test_one <- t.test(df$Total_Amount, mu = 50000)

print(t_test_one)
