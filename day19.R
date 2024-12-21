library(tidyverse)
source("utils.R")
#devtools::install_github('dirmeier/datastructures')
library(astar) #devtools::install_github('machow/astar-r')

input <- "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb" |> str_split_1("\n")

input <- read_lines("data/day_19/input.txt")

colours <- c("w", "u", "b", "r", "g")

cmap <- function(v) {
  lapply(v, \(x) match(x, colours)) |> unlist()
}

patterns <- input[[1]] |> str_split_1(", ") |> str_split("") |> lapply(cmap)

designs <- input[3:length(input)] |> str_split("") |> lapply(cmap)

match_length <- function(design, pattern) {
  if (length(pattern) > length(design)) return(numeric(0))
  if (all(head(design, length(pattern)) == pattern)) return(length(pattern))
  return(numeric(0))
}

find_matches <- function(design, patterns) {
  match_lengths <- unlist(lapply(patterns, \(x) match_length(design, x)))
  lapply(match_lengths, \(x) if(x == 0) numeric(0) else tail(design, -x))
}

cost_estimate <- \(n, g) as.numeric(length(n))

edge_distance <- \(src, dst) 1L

is_goal_reached <- \(n, g) length(n) == 0

hash_func <- \(n) paste0(n, ".", collapse="")

find_combination <- function(design, patterns) {
  neighbors <- \(n) find_matches(n, patterns)
  astar(design, numeric(0), cost_estimate, 
              edge_distance, neighbors, is_goal_reached, 
              hash_func = hash_func)
}

# We might have to reduce the search space first...
remove_na <- \(x) x[which(!is.na(x))]

reduce_patterns <- function(patterns) {
  cur_pat <- patterns
  for (i in 1:length(patterns)) {
    pat <- patterns[[i]]
    if (length(pat) == 1) next
    cur_pat[[i]] <- NA
    if (is.null(find_combination(pat, remove_na(cur_pat)))) cur_pat[[i]] <- pat
    
  }
  remove_na(cur_pat)
}

red_pat <- reduce_patterns(patterns)

answer_1 <- sum(designs |> 
                  lapply(\(x) !is.null(find_combination(x, red_pat))) |>
                  unlist())


# Part 2
# Yikes, astar and reducing the patterns won't work.

