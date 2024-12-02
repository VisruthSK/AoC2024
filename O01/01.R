library(tidyverse)
data <- read_table("./inputs/01.txt", col_names = FALSE)

print(sum(abs(sort(data$X1) - sort(data$X2))))

temp <- tibble(!!!table(data$X2)) |> pivot_longer(cols = everything(), names_to = "X1", values_to = "Count") |> mutate(X1 = as.numeric(X1))

merged <- data |> left_join(temp, by = "X1") |> drop_na()

print(sum(merged$X1 * merged$Count))
