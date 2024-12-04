library(tidyverse)

# read the input data into a matrix
data <- do.call(rbind, read_lines("./inputs/04.txt") |> str_split(""))
example <- do.call(rbind, read_lines("./Day04/example.txt") |> str_split("")) # only used in development

# count occurrences of XMAS both forward and backwards
xmas_count <- function(vec) {
  temp <- paste0(vec, collapse = "")
  sum(str_count(temp, "XMAS") + str_count(temp, "SAMX"))
}

# get every diagonal of m and convert to string
get_all_diagonals <- function(m) {
  n <- nrow(m)
  diagonals <- list()

  for (d in -(n - 3):(n - 3)) {
    diagonals[[as.character(d)]] <- paste0(m[row(m) - col(m) == d], collapse = "")
    diagonals[[paste0("reversed_", as.character(d))]] <- paste0(m[row(m) + col(m) == (n - 1 + d)], collapse = "")
  }

  diagonals
}

# count XMAS (and SAMX) across rows, columns, and diagonals
apply(data, 1, xmas_count) |>
  sum() +
  apply(data, 2, xmas_count) |>
  sum() +
  get_all_diagonals(data) |>
  map(xmas_count) |>
  unlist() |>
  sum()

# check if vector is MAS or SAM
mas_check <- function(vec) {
  paste(vec, collapse = "") %in% c("MAS", "SAM")
}

# check every sub matrix to ensure that MAS or SAM appear on the diagonal
check_sub_matrix <- function(m) {
  mas_check(diag(m)) && mas_check(diag(m[nrow(m):1, ]))
}

# keep running total of every valid submatrix
answer <- 0
for (i in 1:(nrow(data) - 2)) {
  for (j in 1:(ncol(data) - 2)) {
    answer <- answer + check_sub_matrix(data[i:(i + 2), j:(j + 2)])
  }
}
answer
