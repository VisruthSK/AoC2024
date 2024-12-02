library(tidyverse)

data <- read_lines("./inputs/02.txt") |> str_split(" ") |> map(as.numeric)

row_safe <- function(vec){
    temp <- na.omit(vec - lag(vec, 1))

    max_val <- max(abs(temp))
    min_val <- min(abs(temp))

    abs(max_val) <= 3  && abs(min_val) >= 1 && abs(sum(sign(temp))) + 1 == length(vec)
}

print(data |> map(row_safe) |> unlist() |> sum())

row_dampened_safe <- function(vec){
    temp <- na.omit(vec - lag(vec, 1))
    attributes(temp) <- NULL
    
    max_val <- max(abs(temp))
    min_val <- min(abs(temp))
    max_index <- which.max(abs(temp))+1
    min_index <- which.min(abs(temp))+1

    if(abs(max_val) > 3) {
        print("1")
        vec <- vec[-max_index]
        }
    else if (abs(min_val) < 1) {
        print("2")
        vec <- vec[-min_index]
        }
    else if (abs(sum(sign(temp))) + 1 != length(vec)){
        print("3")
        if (sum(sign(temp)) < 0) vec <- vec[-max_index]
        else vec <- vec[-min_index]
    }

    row_safe(vec)
}

# read_lines("./Day02/example.txt") |> str_split(" ") |> map(as.numeric) |> map(row_dampened_safe)
# read_lines("./Day02/test.txt") |> str_split(" ") |> map(as.numeric) |> map(row_dampened_safe)
print(data |> map(row_dampened_safe) |> unlist() |> sum())
