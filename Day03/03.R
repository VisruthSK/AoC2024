library(tidyverse)
data <- paste0(readLines("./inputs/03.txt"), collapse = "")

mult <- function(string_nums) {
  str_split(string_nums, ",") |>
    unlist() |>
    as.numeric() |>
    prod()
}

extract_mult <- function(data) {
  str_extract_all(data, "mul\\(\\d+,\\d+\\)") |>
    unlist() |>
    str_remove("mul\\(") |>
    str_remove("\\)") |>
    map(mult) |>
    unlist() |>
    sum()
}

data |>
  str_remove_all("don't\\(\\).*?do\\(\\)") |>
  extract_mult()

# temp <- data |>
#   str_extract_all("mul\\(\\d+,\\d+\\)|don't\\(\\)|do\\(\\)") |>
#   unlist()

# do <- TRUE
# saved <- c()

# for (entry in temp) {
#   if (entry == "don't()") {
#     do <- FALSE
#   } else if (entry == "do()") {
#     do <- TRUE
#   } else if (do) saved <- c(saved, entry)
# }

# saved |> extract_mult()
