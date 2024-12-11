library(tidyverse)
source("utils.R")

# DAY 9

d9_input <- readLines("data/day_9/input.txt")

# He shows you the disk map (your puzzle input) he's already generated.

# The disk map uses a dense format to represent the layout of files and free 
# space on the disk. The digits alternate between indicating the length of a 
# file and the length of free space.

# Each file on disk also has an ID number based on the order of the files as
# they appear before they are rearranged, starting with ID 0.

# The amphipod would like to move file blocks one at a time from the end of the
# disk to the leftmost free space block (until there are no gaps remaining
# between file blocks).

# The final step of this file-compacting process is to update the filesystem 
# checksum. To calculate the checksum, add up the result of multiplying each of
# these blocks' position with the file ID number it contains. The leftmost block
# is in position 0. If a block contains free space, skip it instead.

#d9_input <- "2333133121414131402"

d9 <- as.numeric(str_split_1(d9_input, ""))

d9_positions <- reduce(
  d9, 
  function(x1, x2) {
    if (length(x1) == 1) x1 <- c(0, x1)
    c(x1, tail(x1, 1) + x2)
    }) |> head(-1)

d9_df <- tibble(count = d9, checksum_pos = d9_positions) %>% 
  mutate(data = row_number() %% 2 == 1) %>%
  group_by(data) %>% mutate(
    id = row_number()-1
  ) %>% ungroup() %>% filter(count != 0) %>% rowwise() %>% 
  mutate(
    checksum_pos = list(checksum_pos:(checksum_pos+count-1))
  ) %>% unnest(c(checksum_pos))

d9_data <- d9_df %>% filter(data == T) %>% arrange(desc(id))

r <- 1
checksum <- 0
for(i in 1:nrow(d9_data)) {
  if (d9_df$data[[i]]) {
    checksum <- checksum + d9_df$checksum_pos[[i]] * d9_df$id[[i]]
  } else {
    checksum <- checksum + d9_df$checksum_pos[[i]] * d9_data$id[[r]]
    r <- r + 1
  }
}

d9_1_answer <- checksum

# The eager amphipod already has a new plan: rather than move individual blocks,
# he'd like to try compacting the files on his disk by moving whole files
# instead.

# This time, attempt to move whole files to the leftmost span of free space 
# blocks that could fit the file. Attempt to move each file exactly once in 
# order of decreasing file ID number starting with the file with the highest 
# file ID number. If there is no span of free space to the left of a file that 
# is large enough to fit the file, the file does not move.

calc_reformat_checksum <- function() {
  ids <- floor(1:length(d9)/2)
  space <- d9[c(F, T)]
  space_pos <- d9_positions[c(F, T)]+1
  
  data <- d9[c(T, F)]
  data_pos <- d9_positions[c(T, F)]+1
  data_ids <- ids[c(T, F)]
  
  build_output <- rep(0, sum(d9))
  
  i <- 0
  while (i < length(data)) {
    i <- i + 1
    r <- length(data) - i + 1
    file <- data[[r]]
    
    # Check if there is a space for the file
    ii <- 1
    found <- F
    while (space_pos[[ii]] < data_pos[[r]]) {
      if (space[[ii]] >= file) {
        found <- T
        break
      }
      ii <- ii + 1
    }
    
    if (found) build_output[space_pos[[ii]]:(space_pos[[ii]]+file-1)] <- data_ids[[r]]
    else build_output[data_pos[[r]]:(data_pos[[r]]+file-1)] <- data_ids[[r]]
    space[[ii]] <- space[[ii]] - file
    space_pos[[ii]] <- space_pos[[ii]] + file
  }
  
  sum(build_output * 0:(length(build_output)-1))
}

d9_2_answer <- calc_reformat_checksum()


# Day 10

d10_input <- readLines("data/day_10/input.txt")

# d10_input <- "89010123
# 78121874
# 87430965
# 96549874
# 45678903
# 32019012
# 01329801
# 10456732" |> str_split_1("\n")

d10 <- read_map_df(d10_input, fun=as.numeric)

d10_m <- read_map_matrix(d10_input, fun=as.numeric)

d10_th <- d10 %>% filter(symbol == 0)

trailhead_score <- function(x, y, mat, unique = T) {
  trailhead_count <- function(x, y, value) {
    if (x <= 0) return(c())
    if (y <= 0) return(c())
    if (x > ncol(mat)) return(c())
    if (y > nrow(mat)) return(c())
    if (mat[y, x] != value) return(c())
    if (value == 9) return(x+y*ncol(mat))
    return(c(
      trailhead_count(x, y-1, value+1),
      trailhead_count(x, y+1, value+1),
      trailhead_count(x+1, y, value+1),
      trailhead_count(x-1, y, value+1)
    ))
  }
  if (unique) return(length(unique(trailhead_count(x, y, 0))))
  length(trailhead_count(x, y, 0))
}


d10_th_scores <- mapply(
  function(x,y) {trailhead_score(x,y,d10_m) },
  d10_th$x,
  d10_th$y)

d10_1_answer <- sum(d10_th_scores)


# PART 2

d10_th_ratings <- mapply(
  function(x,y) {trailhead_score(x, y, d10_m, unique=F) },
  d10_th$x,
  d10_th$y)

d10_2_answer <- sum(d10_th_ratings)


# DAY 11

# As you observe them for a while, you find that the stones have a consistent 
# behavior. Every time you blink, the stones each simultaneously change 
# according to the first applicable rule in this list:
  
# - If the stone is engraved with the number 0, it is replaced by a stone 
#     engraved with the number 1.
# - If the stone is engraved with a number that has an even number of digits, 
#     it is replaced by two stones. The left half of the digits are engraved on 
#     the new left stone, and the right half of the digits are engraved on the 
#     new right stone.
# - If none of the other rules apply, the stone is replaced by a new stone; the 
#     old stone's number multiplied by 2024 is engraved on the new stone.

d11 <- "125 17" |> str_split_1(" ") |> as.numeric()
d11 <- readLines("data/day_11/input.txt") |> str_split_1(" ") |> as.numeric()

d11_df <- tibble(val = d11, n=1) %>% group_by(val) %>% 
  summarize(n = sum(n))

n_digits <- function(x) {
  case_when(
    x == 0 ~ 1,
    T ~ floor(log10(x)) + 1
  )
}

n_split_1 <- function(x, digits) {
  as.integer(floor(x/(10^(digits/2))))
}

n_split_2 <- function(x, digits) {
  half_d <- 10^(digits/2)
  decimal <- x/half_d
  round(half_d * (decimal - trunc(decimal)))
}

blink_2 <- function(df) {
  df <- df %>% mutate(digits = n_digits(val), even = digits %% 2 == 0)
  even <- filter(df, even)
  odd <- filter(df, !even) %>% select(val, n)
  
  new_vals_1 <- even %>% mutate(val = n_split_1(val, digits)) %>% select(val, n)
  new_vals_2 <- even %>% mutate(val = n_split_2(val, digits)) %>% select(val, n)
  
  mutate(odd,
    val = case_when(
      val == 0 ~ 1,
      T ~ val*2024
    )) %>% 
    add_row(new_vals_1) %>% 
    add_row(new_vals_2) %>% 
    group_by(val) %>% 
    summarize(n = sum(n))
}

d11_1_answer <- sum(reapply(blink_2, d11_df, 25)$n)
d11_2_answer <- sum(reapply(blink_2, d11_df, 75)$n)
