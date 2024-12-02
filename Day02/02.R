library(tidyverse)

data <- read_lines("./inputs/02.txt") |> str_split(" ") |> map(as.numeric)

row_safe <- function(vec){
    temp <- na.omit(vec - lag(vec, 1))

    max_val <- max(abs(temp))
    min_val <- min(abs(temp))

    abs(max_val) <= 3  && abs(min_val) >= 1 && abs(sum(sign(temp))) + 1 == length(vec)
}

data |> map(row_safe) |> unlist() |> sum()

row_dampened_safe <- function(vec){
    temp <- na.omit(vec - lag(vec, 1))
    attributes(temp) <- NULL
    
    # print(temp)
    max_val <- max(abs(temp))
    min_val <- min(abs(temp))
    max_index <- which.max(temp)+1
    min_index <- which.min(temp)+1

    print(temp)
    
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

    # print(temp)
    # print(vec)
    
    # print(temp)

    # for (i in 2:length(vec)){
    #     print(vec[i])
    # }

    # print("NEXT")

    # row_safe(vec) || ()
    # abs(sum(sign(temp))) + 1 >= length(vec) - 1

    # print(vec)
    row_safe(vec)
}

# read_lines("./Day02/test.txt") |> str_split(" ") |> map(as.numeric) |> map(row_dampened_safe)
data |> map(row_dampened_safe) |> unlist() |> sum()
