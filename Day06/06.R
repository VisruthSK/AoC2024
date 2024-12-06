library(tidyverse)

# temp <- readLines("./Day06/example.txt")
temp <- readLines("./inputs/06.txt")

border <- length(temp)
guard_map <- temp |>
  str_split("") |>
  unlist() |>
  matrix(nrow = border, byrow = TRUE)

start <- which(guard_map == "^", arr.ind = TRUE) |> as.numeric()
pos <- start
move <- c(-1, 0)
turn <- function(move) c(move[2], -move[1])

while (TRUE) {
  guard_map[pos[1], pos[2]] <- "X"
  temp <- pos + move

  if (guard_map[temp[1], temp[2]] == "#") move <- turn(move)

  pos <- pos + move
}

sum(guard_map == "X")

reset_guard_map <- function() {
  pos <<- start
  move <<- c(-1, 0)
  guard_map[guard_map == "X"] <<- "."
  guard_map[start[1], start[2]] <<- "^"
}

reset_guard_map()

is_loop <- function(guard_map, O, pos, move) {
  guard_map_copy <- guard_map
  guard_map_copy[O[1], O[2]] <- "#"

  visited_positions <- list()

  tryCatch(
    {
      while (TRUE) {
        current_state <- list(position = pos, direction = move)
        if (any(sapply(visited_positions, function(x) identical(x, current_state)))) {
          return(TRUE)
        }

        visited_positions <- append(visited_positions, list(current_state))

        temp <- pos + move

        if (guard_map_copy[temp[1], temp[2]] == "#") {
          move <- turn(move)
        }

        pos <- pos + move
      }
    },
    error = function(cnd) {
      return(FALSE)
    }
  )
}

# should work, just slow.
library(furrr)
plan(multisession)
temp <- which(guard_map == ".", arr.ind = TRUE)
future_map2_lgl(temp[, 1], temp[, 2], ~ is_loop(guard_map, c(.x, .y), pos, move), .progress = TRUE) |> sum()
