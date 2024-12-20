library(tidyverse)
#devtools::install_github('dirmeier/datastructures')
library(astar) #devtools::install_github('machow/astar-r')
source("utils.R")

input <- "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################" |> str_split_1("\n")

input <- read_lines("data/day_16/input.txt")

map <- read_map_matrix(input)

df <- read_map_df(input)

start <- c(df |> filter(symbol=="S") |> select(x,y) |> unlist(), d=1)
goal <- c(df |> filter(symbol=="E") |> select(x,y) |> unlist(), d=4)

#'  neighbors <- function(node) names(nodes[[node]])
#'  cost_estimate <- function(node, goal) 1
#'  edge_distance <- function(src, dst) nodes[[src]][dst]
#'  is_goal_reached <- function(node, goal) identical(node, goal)

dirs <- list(c(1, 0, 0), c(0, 1, 0), c(-1, 0, 0), c(0, -1, 0))

move <- \(n, d) mapply(`+`, n, dirs[[d]])

turn_cw <- \(x) (x) %% 4 + 1
turn_ccw <- \(x) (x - 2) %% 4 + 1

get_symbol <- function(node) {
  map[node[[2]], node[[1]]]
}

neighbors <- function(node) {
   out <- list(
    c(x=node[[1]], y=node[[2]], d=turn_cw(node[[3]])),
    c(x=node[[1]], y=node[[2]], d=turn_ccw(node[[3]])),
    move(node, node[[3]])
  )
  if (get_symbol(out[[3]]) == "#") out <- out[1:2]
  out
}

cost_estimate <- \(n, g) sum(abs(head(mapply(`-`, n, g), 2)))

edge_distance <- \(src, dst) if (src[[3]] != dst[[3]]) 1000 else 1

is_goal_reached <- \(n, g) identical(head(n, 2), head(g,2))

hash_func <- \(n) paste0(n, collapse=".")

path <- astar(start, goal, cost_estimate, 
              edge_distance, neighbors, is_goal_reached, 
              hash_func = hash_func)

calc_cost <- function(path) {
  df <- reduce(path, rbind) |> data.frame() |> as_tibble() 
  moves <- df |> group_by(paste(x, y)) |> slice(1L) |> nrow() -1
  turns <- df |> mutate(diff = d - lag(d)) |> filter(diff != 0) |> nrow()
  moves+turns*1000
}

answer_1 <- calc_cost(path)


## PART 2

# We redifine the astar algorithm to collect all paths found

make_search_node <- function(data, gscore, fscore) {
  env <- new.env()
  env$data = data
  env$gscore = gscore
  env$fscore = fscore
  env$closed = FALSE
  env$out_openset = TRUE
  env$came_from = list()
  env
}

#env_names()

reconstruct_path <- function(goal) {
  uniq <- new.env()
  visited <- new.env()
  crnt <- goal
  
  .reconstruct_path <- function(crnt) {
    while(length(crnt$came_from)>0) {
      hash_1 <- paste(head(crnt$data, 2), collapse=".")
      hash_2 <- paste(crnt$data, collapse=".")
      if (!is.null(visited[[hash_2]])) return()
      
      visited[[hash_2]] <- T
      uniq[[hash_1]] <- T
      if (length(crnt$came_from) == 1) crnt <- first(crnt$came_from)
      else walk(crnt$came_from, .reconstruct_path)
    }
  }
  
  .reconstruct_path(crnt)
  names(uniq)
}

# Redefine the astar algorithm from 'machow/astar-r'
astar2 <- function(start, goal,
                   cost_estimate, edge_distance, neighbors, is_goal_reached,
                   hash_func = identity, search_node_env = NULL) {
  if (is_goal_reached(start, goal))
    return(list(start))
  
  search_nodes <- if (!is.null(search_node_env)) search_node_env else list()
  
  start_node <- make_search_node(start, gscore = 0, fscore = cost_estimate(start, goal))
  start_hash <- hash_func(start)
  search_nodes[[start_hash]] <- start_node
  
  open_set <- datastructures::binomial_heap("numeric")
  # prioritize nodes based on crnt estimated cost from origin to goal
  # through that node (fscore)
  datastructures::insert(open_set, start_node$fscore, start_hash)
  
  goal_out <- NULL
  max_score <- Inf
  
  while (!is.null(datastructures::peek(open_set))) {
    crnt <- search_nodes[[datastructures::pop(open_set)[[1]]]]
    
    if (is_goal_reached(crnt$data, goal)) {
      if (is.null(goal_out)) goal_out <- crnt
      if (crnt$gscore < max_score) max_score <- crnt$gscore
      next
    } else {
      crnt$closed <- TRUE
    }
    
    if (crnt$gscore > max_score) next

    crnt$out_openset <- TRUE
    
    # nodes need to be hashable
    for (neighbor in neighbors(crnt$data)) {
      indx <- hash_func(neighbor)
      neigh_node <- search_nodes[[indx]]
      if (is.null(neigh_node)) {
        neigh_node <- search_nodes[[indx]] <- make_search_node(neighbor, Inf, Inf)
      }
      
      # skip if this new path through neighbor has higher cost (to neighbor)
      tentative_gscore <- crnt$gscore + edge_distance(crnt$data, neigh_node$data)
      if (tentative_gscore > neigh_node$gscore) next
      
      if (tentative_gscore == neigh_node$gscore) {
        neigh_node$came_from <- append(neigh_node$came_from, crnt)
        next
      }
      
      if (neigh_node$closed) next
      
      # update with new path, and estimated cost
      neigh_node$came_from <- list(crnt)
      neigh_node$gscore <- tentative_gscore
      neigh_node$fscore <- tentative_gscore + cost_estimate(neigh_node$data, goal)
      
      # put back into open set, using updated score
      if (neigh_node$out_openset) {
        neigh_node$out_openset <- FALSE
        datastructures::insert(open_set, neigh_node$fscore, indx)
      }
      
    }
  }
  return(reconstruct_path(goal_out))
}

search_nodes = new.env()

visited_tiles <- astar2(start, goal, cost_estimate, edge_distance, neighbors, 
                        is_goal_reached, hash_func = hash_func,
                        search_node_env = search_nodes)

hash_simp <- \(n) paste(head(n, 2), collapse=".")

answer_2 <- length(c(visited_tiles, hash_simp(start), hash_simp(goal)) |> 
                     unique())

# TMP

lists <- visited_tiles |> str_split("\\.") |> lapply(as.numeric)
dat <- data.frame(do.call(rbind, lists)) |> as_tibble() |>
  transmute(x=X1, y=X2, symbol="#")

print_map_df(dat)
