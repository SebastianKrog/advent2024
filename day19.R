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

#answer_1 <- sum(designs |> 
#                  lapply(\(x) !is.null(find_combination(x, red_pat))) |>
#                  unlist())


# Part 2
# Yikes, astar and reducing the patterns won't work.

library(memoise) # We need depth_first search with caching...

# First, we should cache the match_finding.
# We only need to keep track of number of combinations
# Finally, we cache the count calculation on subpatterns

longest_pattern <- patterns |> map(\(x) length(x)) |> unlist() |> max()
# = 8 for full set and 3 for test

.find_matches <- function(d) {
  if (length(d) >= longest_pattern) return(find_matches(d, patterns))
  find_matches(d, patterns[which(
    unlist(lapply(patterns, \(p) length(p) <= length(d))))])
}

cache_find_matches <- memoise(.find_matches)

c_find_matches <- function(design) {
  des <- head(design, longest_pattern)
  matches <- cache_find_matches(des)
  lapply(matches, \(m) c(m, tail(design, -longest_pattern)))
}

find_combination2 <- function(design) {
  neighbors <- \(n) cache_find_matches(n)
  astar(design, numeric(0), cost_estimate, 
        edge_distance, neighbors, is_goal_reached, 
        hash_func = hash_func)
}

designs_valid <- designs |> 
                  lapply(\(x) !is.null(find_combination2(x))) |>
                  unlist()

calc_combinations <- function(design) {
  seen <- new.env()
  
  new_node <- function(data, came_from) {
    env <- new.env()
    env$data = data
    env$score = NA
    if (is.null(came_from)) env$came_from = list()
    else env$came_from = list(came_from)
    env
  }
  
  neighbors <- \(n) c_find_matches(n)
  
  crnt <- new_node(design, NULL)
  seen[[hash_func(design)]] <- crnt
  
  .calc_comb <- function(crnt) {
    for (neighbor in neighbors(crnt$data)) {
      neigh_hash <- hash_func(neighbor)
      neigh_search <- seen[[neigh_hash]]
      if (is.null(neigh_search)) {
        neigh_node <- new_node(neighbor, crnt)
        seen[[neigh_hash]] <- neigh_node
        .calc_comb(neigh_node)
      } else {
        # Check for unique?
        neigh_search$came_from <- append(neigh_search$came_from, crnt) |>
          unique()
      }
    }
  }
  
  .reconstruct_path <- function(crnt) {
    uniq <- new.env()
    
    .rec_path <- function(crnt) {
      while(T) {
        if (length(crnt$came_from)==0) {
          crnt$score <- 1
          return(1)
        }
        hash <- hash_func(crnt$data)
        if (is.null(uniq[[hash]])) uniq[[hash]] <- T
        
        not_seen_before <- unlist(
          lapply(crnt$came_from, 
                 \(x) is.null(uniq[[hash_func(x$data)]])))
        
        uniq_from <- 
          crnt$came_from[which(not_seen_before)]
        
        prev_score <- 
          sum(map(crnt$came_from[which(!not_seen_before)], 
                  \(x) x$score) |> unlist())
    
        score <- sum(map(uniq_from, .rec_path) |> unlist()) + prev_score
        crnt$score <- score
        return(score)
      }
    }
    
    return(.rec_path(crnt))
  }
  
  .calc_comb(crnt)
  
  #seen[["."]]
  .reconstruct_path(seen[["."]])
}

sums <- designs[which(designs_valid)] |> lapply(calc_combinations) |> unlist()

answer_2 <- sum(sums)
