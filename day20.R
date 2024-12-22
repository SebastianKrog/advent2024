library(tidyverse)
source("utils.R")
#devtools::install_github('dirmeier/datastructures')
library(astar) #devtools::install_github('machow/astar-r')

input <- "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############" |> str_split_1("\n")

input <- read_lines("data/day_20/input.txt")

mat <- read_map_matrix(input)

df <- read_map_df(input, ignore = NULL)

start <- df |> filter(symbol=="S") %>% select(x, y) |> as.list() |> unlist()

goal <- df |> filter(symbol=="E") %>% select(x, y)  |> as.list() |> unlist()

cost_estimate <- \(n, g) 1
edge_distance <- \(src, dst) 1
is_goal_reached <- \(n, g) all(n == g)
hash_func <- \(n) paste0(n, collapse=".")
neighbors <- function(n) {
  x <- n[[1]]
  y <- n[[2]]
  list(
    if (mat[y,x+1] != "#") c(x + 1, y),
    if (mat[y,x-1] != "#") c(x - 1, y),
    if (mat[y+1,x] != "#") c(x, y + 1),
    if (mat[y-1,x] != "#") c(x, y - 1)
  ) |> discard(is.null)
}

path <- astar(start, goal, cost_estimate, 
              edge_distance, neighbors, is_goal_reached, 
              hash_func = hash_func)

path_df <- reduce(path, rbind) |> data.frame() |> as_tibble() |> 
  mutate(dist = row_number())

cheats_df <- path_df |>
  group_by(x) |>
  arrange(y) |> 
  mutate(
    diffy = y-lag(y), 
    diffdist_y = abs(dist-lag(dist))-2) |>
  group_by(y) |>
  arrange(x) |> 
  mutate(
    diffx = x-lag(x),
    diffdist_x = abs(dist-lag(dist))-2,
  ) |> ungroup()

cheats_x <- cheats_df |> filter(diffx == 2) |> 
  mutate(cheat_score = diffdist_x)

cheats_y <- cheats_df |> filter(diffy == 2) |> 
  mutate(cheat_score = diffdist_y)

cheats <- add_row(cheats_x, cheats_y) |> 
  select(x, y, cheat_score) |> 
  arrange(desc(cheat_score))

answer_1 <- cheats |> filter(cheat_score >= 100) |> count()


distance <- function(x1, y1, x2, y2) {
  abs(x2-x1)+abs(y2-y1)
}

calc_cheats <- function(pos, pos_dist, limit = 100) {
  x1 <- pos[[1]]
  y1 <- pos[[2]]
  # FIND CHEATS FROM POS
  found <- path_df |> 
    mutate(dist_to_pos = distance(x1, y1, x, y)) |> 
    filter(dist_to_pos <= 20, dist_to_pos > 0) |>
    mutate(time_saved = dist - pos_dist - dist_to_pos) |>
    #filter(time_saved >= limit)
    pull(time_saved)
  sum(found >= limit)
}

cheat_counts <- pmap(list(path, 1:length(path)), calc_cheats)

answer_2 <- cheat_counts |> unlist() |> sum()
