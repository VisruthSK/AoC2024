library(tidyverse)

dataa <- readLines("./inputs/05a.txt")
datab <- readLines("./inputs/05b.txt")

# dataa <- readLines("./Day05/examplea.txt")
# datab <- readLines("./Day05/exampleb.txt")

mapping <- dataa |>
  as.tibble() |>
  separate(value, into = c("inputs", "outputs"), sep = "\\|") |>
  group_by(inputs) |>
  summarize(outputs = list(as.numeric(outputs)), .groups = "drop")

extract_middle_correct <- function(vec) {
  l <- length(vec)
  for (i in 1:(l - 1)) {
    temp <- mapping |>
      filter(inputs == vec[i]) |>
      pull(outputs) |>
      unlist()
    rest <- vec[(i + 1):l]

    if (!all(map_lgl(rest, ~ .x %in% temp))) {
      return(FALSE)
    }
  }

  vec[ceiling(l / 2)]
}

# extract_middle_correct(c(75,47,61,53,29))

datab |>
  map(~ as.numeric(str_split(.x, ",")[[1]])) |>
  map_dbl(~ extract_middle_correct(.x)) |>
  sum()

extract_middle_incorrect <- function(vec) {
  if (extract_middle_correct(vec) != FALSE) {
    return(0)
  }

  l <- length(vec)
  while (extract_middle_correct(vec) == FALSE) {
    # print(vec)
    for (i in 1:(l - 1)) {
      temp <- mapping |>
        filter(inputs == vec[i]) |>
        pull(outputs) |>
        unlist()

      for (j in (i + 1):l) {
        if (vec[j] %in% temp) {
          next
        } else {
          vec <- c(vec[j], vec[-j])
          break
        }
      }
    }
  }

  extract_middle_correct(vec)
}

# extract_middle_incorrect(c(75, 97, 47, 61, 53))

datab |>
  map(~ as.numeric(str_split(.x, ",")[[1]])) |>
  map_dbl(~ extract_middle_incorrect(.x)) |>
  sum()
