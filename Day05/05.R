library(tidyverse)

dataa <- readLines("./inputs/05a.txt")
datab <- readLines("./inputs/05b.txt")

# dataa <- readLines("./Day05/examplea.txt")
# datab <- readLines("./Day05/exampleb.txt")

mapping <- dataa |> as.tibble() |> separate(value, into = c("inputs", "outputs"), sep = "\\|") |> group_by(inputs) |> summarize(outputs = list(as.numeric(outputs)), .groups = "drop")

extract_middle_correct <- function(vec){
  l <- length(vec)
  for(i in 1:(length(vec) - 1)){
    # print(i)
    temp <- mapping |> filter(inputs == vec[i]) |> pull(outputs) |> unlist()
    rest <- vec[(i+1):l]
    
    # print(temp)
    # print(rest)

    if(!all(map_lgl(rest, ~.x %in% temp))){
      return(0)
    }
  }
  
  vec[ceiling(l/2)]
}

# extract_middle_correct(c(75,47,61,53,29))

datab |> map(~as.numeric(str_split(.x, ",")[[1]])) |> map_dbl(~extract_middle_correct(.x)) |> sum()
