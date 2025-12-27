df <- read.csv("airline_flight_dataset.csv")

str(df)
colnames(df)

df$delay_before <- df$Arrival_Delay
df$delay_after  <- df$Arrival_Delay - rnorm(nrow(df), mean = 5, sd = 2)

t_test_paired <- t.test(
  df$delay_before,
  df$delay_after,
  paired = TRUE
)

print(t_test_paired)
