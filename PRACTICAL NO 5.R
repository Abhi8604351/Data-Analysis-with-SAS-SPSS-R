library(dplyr)

mobile <- read.csv("Processed_Global_Mobile_Prices.csv")

mobile_sorted_price <- mobile |>
  arrange(PriceUSD)

head(mobile_sorted_price, 5)

mobile_sorted_ram_desc <- mobile |>
  arrange(desc(RAM))

head(mobile_sorted_ram_desc, 5)

mobile_multi_sort <- mobile |>
  arrange(Brand, desc(PriceUSD))

head(mobile_multi_sort, 10)

high_battery_sorted <- mobile |>
  filter(Battery > 5000) |>
  arrange(Rating)

cat("Top 5 high-battery phones with lowest rating:\n")
print(high_battery_sorted |> select(Battery, Rating, PriceUSD) |> head(5))