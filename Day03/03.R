library(tidyverse)
data <- paste0(readLines("./inputs/03.txt"), collapse = "")

mult <- function(string_nums) str_split(string_nums, ",") |> unlist() |> as.numeric() |> prod()

extract_mult <- function(data) str_extract_all(data, "mul\\(\\d+,\\d+\\)") |> unlist() |> str_remove("mul\\(") |> str_remove("\\)") |> map(mult) |> unlist() |> sum()

data |> str_remove_all("don't\\(\\).*do\\(\\)") |>  str_remove_all("don't\\(\\).*$") |> extract_mult()

"xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" |> str_remove_all("don't\\(\\).*do\\(\\)") |>  str_remove("don't\\(\\).*$") |> extract_mult()