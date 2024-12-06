library(tidyverse)

temp <- readLines("./inputs/06.txt")
# temp <- readLines("./Day06/example.txt")

border <- length(temp)
data <- temp |>
  str_split("") |>
  unlist() |>
  matrix(nrow = border, byrow = TRUE)

pos <- which(data == "^", arr.ind = TRUE) |> as.numeric()
# x <- pos[1]
# y <- pos[2]

move <- c(-1, 0)
turn <- function(move) c(move[2], -move[1])

# valid_pos <- function(pos) {
#   x <- pos[1]
#   y <- pos[2]
#   x <= border && x > 0 && y <= border && y > 0
# }

while (TRUE) {
  data[pos[1], pos[2]] <- "X"
  temp <- pos + move

  if (data[temp[1], temp[2]] == "#") {
    move <- turn(move)
  }

  pos <- pos + move
  print(pos)
}

sum(data == "X")
