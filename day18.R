library(tidyverse)
source("utils.R")
#devtools::install_github('dirmeier/datastructures')
library(astar) #devtools::install_github('machow/astar-r')

input <- "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0" |> str_split_1("\n")

input <- read_lines("data/day_18/input.txt")

df <- str_split(input, ",") |> reduce(rbind) |> data.frame() |> as_tibble() |>
  transmute(x=as.numeric(X1)+1,y=as.numeric(X2)+1, n=row_number())

dirs <- list(c(1, 0), c(0, 1), c(-1, 0), c(0, -1))

in_bounds <- function(n, max=70) {
  if (n[[1]]<0) return(F)
  if (n[[1]]>max) return(F)
  if (n[[2]]<0) return(F)
  if (n[[2]]>max) return(F)
  return(T)
}

get_adj <- function(n, mat, max=70) {
  out <- dirs |> lapply(\(N) mapply(`+`, n, N))
  ib <- out[which(lapply(out, \(x) in_bounds(x, max)) |>unlist())]
  ib[which(lapply(ib, \(x) mat[x[[2]]+1,x[[1]]+1] != "#") |>unlist())]
}

cost_estimate <- \(n, g) sum(abs(mapply(`-`, n, g)))

edge_distance <- \(src, dst) 1

is_goal_reached <- \(n, g) identical(n, g)

get_path <- function(limit=1024, start=c(0,0), goal=c(70,70)) {
  kb <- df |> slice_head(n=limit) |> mutate(symbol = "#")
  mat <- df_to_mat(kb)
  
  neighbors <- function(node) {
    get_adj(node, mat, max(goal))
  }
  
  out <- astar(start, goal, cost_estimate, 
        edge_distance, neighbors, is_goal_reached, 
        hash_func = hash_func)
  
  out
}

path <- get_path()

answer_1 <- length(path)-1
answer_1

# Part 2

df$hash <- mapply(\(x,y) hash_func(c(x,y)), df$x-1, df$y-1)

find_in_path <- function(lower, path) {
  hash_path <- lapply(path, hash_func)
  in_path <- df$hash[lower:length(df$hash)] %in% hash_path
  df$n[lower:length(df$hash)][which(in_path)]
}

binary_search <- function() {
  # We continuously look at potential blocking timepoints, and pick the median
  path <- get_path()
  lower <- 1024
  upper <- max(df$n)
  potentials <- find_in_path(lower, path)
  while (upper + 1 > lower) {
    pot_low <- potentials[1:ceiling(length(potentials)/2)]
    cur <- tail(pot_low, 1)
    path <- get_path(cur)
    
    while (is.null(path)) { # Find a lower value
      upper <- cur
      if (length(pot_low) == 1) {
        if (is.null(get_path(pot_low))) return(pot_low)
        else return(upper)
      }
      pot_low <- pot_low[1:ceiling(length(pot_low)/2)]
      cur <- tail(pot_low, 1)
      path <- get_path(cur)
    } 
    
    lower <- cur
    potentials <- find_in_path(lower, path)
    potentials <- potentials[which(potentials < upper)]
  }
}

block_limit <- binary_search()

coords <- df %>% filter(n==block_limit) |> transmute(x=x-1, y=y-1) |> unlist()

answer_2 <- paste(coords, collapse=",")
