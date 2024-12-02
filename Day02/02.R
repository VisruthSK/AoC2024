library(tidyverse)

data <- read_lines("./inputs/02.txt") |>
  str_split(" ") |>
  map(as.numeric)

row_safe <- function(vec) {
  temp <- na.omit(vec - lag(vec, 1))

  max_val <- max(abs(temp))
  min_val <- min(abs(temp))

  abs(max_val) <= 3 && abs(min_val) >= 1 && abs(sum(sign(temp))) + 1 == length(vec)
}

print(data |> map(row_safe) |> unlist() |> sum())

row_dampened_safe <- function(vec) {
  for (i in seq_len(length(vec))) {
    if (row_safe(vec[-i])) {
      return(TRUE)
    }
  }
  FALSE
}

print(data |> map(row_dampened_safe) |> unlist() |> sum())
