library(tidyverse)
source("utils.R")

input <- "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####" |> str_split_1("\n")

input <- read_lines("data/day_25/input.txt")

entries <- split(input, 
                 case_when(1:length(input) %% 8 == 0 ~ NA, 
                           T ~ floor(1:length(input)/8)))

parse_key_lock <- function(lines) { 
  x <- str_split(lines, "") |> map(\(x) x == "#")
  mapply(sum, x[[1]], x[[2]], x[[3]], x[[4]], x[[5]], x[[6]], x[[7]]) - 1
}

locks <- entries[which(map(entries, \(x) tail(x, 1)) == ".....")] |> 
  map(parse_key_lock) #|> map(\(x) 
keys <- entries[which(map(entries, \(x) tail(x, 1)) == "#####")] |> 
  map(parse_key_lock)

key_fits <- function(key) {
  sum(map(locks, \(lock) sum(6 - key <= lock)) == 0)
}

answer_1 <-sum(unlist(map(keys, key_fits)))

