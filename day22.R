library(tidyverse)
source("utils.R")

input <- "1
10
100
2024" |> str_split_1("\n") |> as.numeric()

input <- 123

mix <- function(sec, val) {
  bitwXor(sec, val)
}

prune <- function(sec) {
  sec %% 16777216
}

next_sec <- function(sec) {
  sec <- prune(mix(sec, sec*64))
  sec <- prune(mix(sec, trunc(sec/32)))
  prune(mix(sec, sec*2048))
}

reapply(next_sec, 123, 10)

# Ugh... we hit the 2^31 limit again... R is not good with bitwise ops on
# large numbers...