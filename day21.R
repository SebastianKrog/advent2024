library(tidyverse)
source("utils.R")
library(memoise)

input <- "029A
980A
179A
456A
379A" |> str_split_1("\n") 

input <- read_lines("data/day_21/input.txt")

codes <- input |> str_split("")

num_input <- str_match(input, "\\d+")[,1] |> as.numeric()


##     +---+---+
##     | ^ | A |
## +---+---+---+
## | < | v | > |
## +---+---+---+
dir_pad_edge_path <- function(src, dst) {
  if (src == dst) return("A")
  if (src == "A") {
    switch (dst,
      "<" = c("v", "<", "<", "A"),
      ">" = c("v", "A"),
      "^" = c("<", "A"),
      #"v" = c("v", "<", "A")
      "v" = c("<", "v", "A")
    )
  } else if (src == "<") {
    switch (dst,
      "A" = c(">", ">", "^", "A"),
      ">" = c(">", ">", "A"),
      "^" = c(">", "^", "A"),
      "v" = c(">", "A")
    )
  } else if (src == ">") {
    switch (dst,
      "A" = c("^", "A"),
      "<" = c("<", "<", "A"),
      "^" = c("<", "^", "A"),
      "v" = c("<", "A")
    )
  } else if (src == "^") {
    switch (dst,
      "A" = c(">", "A"),
      "<" = c("v", "<", "A"),
      ">" = c("v", ">", "A"),
      "v" = c("v", "A")
    )
  } else if (src == "v") {
    switch (dst,
      #"A" = c(">", "^", "A"),
      "A" = c("^", ">", "A"),
      "<" = c("<", "A"),
      ">" = c(">", "A"),
      "^" = c("^", "A")
    )
  } else stop(paste("Edge not found between ", src, dst))
}

dir_pad_build <- function(src, dst) {
  c(src, dir_pad_edge_path(tail(src, 1), dst))
}

dir_pad_walk <- function(vct) {
  pmap(list(lag(vct, 1, "A"), vct), dir_pad_edge_path) |> unlist()
}

dir_pad_edge_meta <- function(src, dst, level) {
  path <- dir_pad_edge_path(src, dst)
  while(level > 1) {
    path <- dir_pad_walk(path)
    level <- level - 1
  }
  path
}

dir_pad_level2 <- memoise(function(src, dst) {
  length(dir_pad_edge_meta(src, dst, 2))
})

##  +---+---+---+
##  | 7 | 8 | 9 |
##  +---+---+---+
##  | 4 | 5 | 6 |
##  +---+---+---+
##  | 1 | 2 | 3 |
##  +---+---+---+
##      | 0 | A |
##      +---+---+
np <- c("0", "A", as.character(1:9))
np_col <- c(2, 3, 1:3, 1:3, 1:3)
np_row <- c(4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1)

numpad_path <- function(src, dst) {
  np_dst <- which(np == dst)
  np_src <- which(np == src)
  
  # Prioritize column
  .np_path <- function(sr, ds) {
    if (sr == ds) return("A")
    if (np_row[sr] < np_row[ds]) return(c("v", .np_path(sr-3, ds)))
    if (np_row[sr] > np_row[ds]) return(c("^", .np_path(sr+3, ds)))
    if (np_col[sr] < np_col[ds]) return(c(">", .np_path(sr+1, ds)))
    if (np_col[sr] > np_col[ds]) return(c("<", .np_path(sr-1, ds)))
  }
  
  # Prioritize row
  .np_path2 <- function(sr, ds) {
    if (sr == ds) return("A")
    if (np_col[sr] < np_col[ds]) return(c(">", .np_path2(sr+1, ds)))
    if (np_col[sr] > np_col[ds]) return(c("<", .np_path2(sr-1, ds)))
    if (np_row[sr] < np_row[ds]) return(c("v", .np_path2(sr-3, ds)))
    if (np_row[sr] > np_row[ds]) return(c("^", .np_path2(sr+3, ds)))
  }
  
  # When going left and up, prioritize left when possible.
  if (!any(c("0", "A") %in% c(src, dst))) {
    if (np_col[np_dst] < np_col[np_src] & np_row[np_dst] < np_row[np_src]) .np_path2(np_src, np_dst)
    else if (np_col[np_dst] > np_col[np_src] & np_row[np_dst] < np_row[np_src]) .np_path(np_src, np_dst)
    else .np_path2(np_src, np_dst)
  }
  else if (dst %in% c("8", "5", "2")) .np_path2(np_src, np_dst)
  else if (src %in% c("7", "4", "1")) .np_path2(np_src, np_dst)
  else .np_path(np_src, np_dst)
}

code_path <- function(code) {
  pmap(list(lag(code, 1, "A"), code), numpad_path) |> unlist()
}

complexity <- function(code) {
  cp <- code_path(code)
  pmap(list(lag(cp, 1, "A"), cp), dir_pad_level2) |> unlist() |> sum()
}

answer_1 <- sum(unlist(map(codes, complexity)) * num_input)


# Part 2
dir_pad_edge_level <- memoise(function(src, dst, level) {
  path <- dir_pad_edge_path(src, dst)
  if (level == 1) return(length(path))
  pmap(list(lag(path, 1, "A"), path), dir_pad_edge_level, level - 1) |> unlist() |> sum()
})

complexity_level <- function(code, level=25) {
  cp <- code_path(code)
  pmap(list(lag(cp, 1, "A"), cp), dir_pad_edge_level, level) |> unlist() |> sum()
}

answer_2 <- sum(unlist(map(codes, complexity_level)) * num_input)

answer_2 |> format(scientific = F)


# NOT 349855514937220 -- too high
# NOT 137813495802036 -- too low