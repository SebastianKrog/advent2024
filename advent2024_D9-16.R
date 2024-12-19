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

# For all practical purposes, this means that a hiking trail is any path that
# starts at height 0, ends at height 9, and always increases by a height of
# exactly 1 at each step.

# A trailhead is any position that starts one or more hiking trails - here, 
# these positions will always have height 0. Assembling more fragments of pages,
# you establish that a trailhead's score is the number of 9-height positions
# reachable from that trailhead via a hiking trail.

# What is the sum of the scores of all trailheads on your topographic map?

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
    if (value == 9) return(x+y*ncol(mat)) # Calc. unique number based on xy
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

# The paper describes a second way to measure a trailhead called its rating. A 
# trailhead's rating is the number of distinct hiking trails which begin at
# that trailhead.

# What is the sum of the ratings of all trailheads?

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

blink <- function(df) {
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

d11_1_answer <- sum(reapply(blink, d11_df, 25)$n)
d11_2_answer <- sum(reapply(blink, d11_df, 75)$n)


# Day 12

d12_input <- "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE" |> str_split_1("\n")

d12_input <- readLines("data/day_12/input.txt")

# Due to "modern" business practices, the price of fence required for a region 
# is found by multiplying that region's area by its perimeter. The total price 
# of fencing all regions on a map is found by adding together the price of 
# fence for every region on the map.

# What is the total price of fencing all regions on your map?

d12 <- read_map_matrix(d12_input)

pl <- function(area, perimeter) {
  c(area=area, perimeter=perimeter)
}

search_plot <- function(x, y, symbol, id) {
  .search_plot <- function(x, y) {
    if (x < 1 || y < 1 || x > ncol(d12) || y > nrow(d12)) return(pl(0,1))
    if (d12[y,x] == as.character(id)) return(c())
    if (d12[y,x] != symbol) return(pl(0,1))
    d12[y,x] <<- id
    return(rbind(
      pl(1,0),
      .search_plot(x + 1, y),
      .search_plot(x, y + 1),
      .search_plot(x - 1, y),
      .search_plot(x, y - 1)
    ))
  }
  as_tibble(rbind(.search_plot(x,y))) %>% summarize(
    id = id,
    symbol = symbol,
    area = sum(area),
    perimeter = sum(perimeter)
  )
}

d12_df <- tibble(id = numeric(), symbol=character(),
                 area = numeric(), perimeter = numeric())

for (y in 1:nrow(d12)) {
  for (x in 1:ncol(d12)) {
    plot <- d12[y,x]
    if (is.na(as.numeric(plot))) {
      id <- max(0, d12_df$id) + 1
      region <- search_plot(x,y,plot,id)
      d12_df <- add_row(d12_df, region)
    }
  }
}

d12_1_answer <- sum(d12_df$area * d12_df$perimeter)

# Under the bulk discount, instead of using the perimeter to calculate the
# price, you need to use the number of sides each region has. Each straight
# section of fence counts as a side, regardless of how long it is.

d12_n <- d12
class(d12_n) <- "numeric"

d12_sides <- tibble(id=numeric())

d12_clean_rows <- function(rows) {
  arrange(rows, dist) %>% 
    group_by(id) %>% 
    mutate(
      diff = dist - lag(dist) # All sides with diff == 1 are continuous
    ) %>% ungroup() %>%
    filter(!is.na(id), is.na(diff) | diff != 1) %>% 
    select(id)
}

# We loop over both "sides" (lines) between each row/col of garden plots.
for (y in 0:(nrow(d12_n))) {
  # For the start and end we add an extra line
  if (y == 0) id_1 <- NA else id_1 <- d12_n[y,]
  if (y == nrow(d12_n)) id_2 <- NA else id_2 <- d12_n[y+1,]
  
  # There are only fences between different (ie. dissimilar) plot ids
  dissimilar = !(id_1 == id_2) | is.na(id_1) | is.na(id_2)
  dist <- (1:ncol(d12_n))[dissimilar]
  
  rows_1 <- tibble(dist, id = id_1[dissimilar]) |> d12_clean_rows()
  rows_2 <- tibble(dist, id = id_2[dissimilar]) |> d12_clean_rows()
  d12_sides <- add_row(d12_sides, rbind(rows_1, rows_2))
}

# In both directions
for (x in 0:(ncol(d12))) {
  if (x == 0) id_1 <- NA else id_1 <- d12_n[,x]
  if (x == nrow(d12_n)) id_2 <- NA else id_2 <- d12_n[,x+1]
  
  dissimilar = !(id_1 == id_2) | is.na(id_1) | is.na(id_2)
  dist <- (1:nrow(d12_n))[dissimilar]
  
  rows_1 <- tibble(dist, id = id_1[dissimilar]) |> d12_clean_rows()
  rows_2 <- tibble(dist, id = id_2[dissimilar]) |> d12_clean_rows()
  d12_sides <- add_row(d12_sides, rbind(rows_1, rows_2))
}

d12_total_sides <- d12_sides %>% 
  count(id) %>%
  left_join(d12_df, by="id")

d12_2_answer <- sum(d12_total_sides$area * d12_total_sides$n)


# Day 15

d15_input <- "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

d15_input <- readLines("data/day_15/input.txt") |> paste0(collapse="\n")

d15_split <- d15_input |> str_split_1("\n\n")

d15_moves <- d15_split[[2]] |> str_split_1("")
d15_moves <- d15_moves[!d15_moves == "\n"]

d15 <- d15_split[[1]] |> str_split_1("\n")
d15_df <- read_map_df(d15)
d15_m <- read_map_matrix(d15)

d15_start <- d15_df %>% filter(symbol=="@") %>% select(x,y) %>% as.list()

moves <- c("<", "^", ">", "v")
dirs <- list(
  list(x=-1,y=0),
  list(x=0,y=-1),
  list(x=1,y=0),
  list(x=0,y=+1)
)

get_dir <- function(d) dirs[which(moves==d)][[1]]

d15_m[d15_start$y, d15_start$x] <- "."

find_next_free_space <- function(pos, move, mat) {
  check_pos <- pos
  while (T) {
    check_pos <- mapply(`+`, check_pos, get_dir(move)) |> as.list()
    check_found <- mat[check_pos$y, check_pos$x]
    if (check_found == "#") { return(NULL) }
    if (check_found == ".") { return(check_pos) }
  }
}

cur_pos <- d15_start
for (move in d15_moves) {
  next_pos <- mapply(`+`,cur_pos, get_dir(move)) |> as.list()
  next_found <- d15_m[next_pos$y, next_pos$x] 
  if (next_found == "#") {
    next
  }
  if (next_found == ".") {
    cur_pos <- next_pos
    next
  }
  if (next_found == "O") {
    next_free_space <- find_next_free_space(next_pos, move, d15_m)
    if (!is.null(next_free_space)) {
      cur_pos <- next_pos
      d15_m[next_pos$y, next_pos$x] <- "."
      d15_m[next_free_space$y, next_free_space$x] <- "O"
    }
  }
}

d15_calc_gps <- function(mat) {
  map_matrix_to_df(mat) %>% 
    filter(symbol %in% c("O", "[")) %>% 
    mutate(
      score = 100*(y-1)+x-1
    ) %>% summarise(sum(score)) %>% pull()
}

d15_1_answer <- d15_calc_gps(d15_m)


d15_m <- read_map_matrix(d15)

d15_m2 <- matrix(nrow=nrow(d15_m), ncol=ncol(d15_m)*2)
for (row in 1:nrow(d15_m)) {
  for (col in 1:ncol(d15_m)) {
    space <- d15_m[row, col]
    if (space == ".") {
      d15_m2[row,col*2-1] <- "."
      d15_m2[row,col*2] <- "."
    }
    if (space == "#") {
      d15_m2[row,col*2-1] <- "#"
      d15_m2[row,col*2] <- "#"
    }
    if (space == "O") {
      d15_m2[row,col*2-1] <- "["
      d15_m2[row,col*2] <- "]"
    }
    if (space == "@") {
      d15_m2[row,col*2-1] <- "."
      d15_start <- list(x = 2*col-1, y=row)
      d15_m2[row,col*2] <- "."
    }
  }
}

move_dir <- function(pos, dir) {
  mapply(`+`,pos, get_dir(dir)) |> as.list()
}

move_boxes <- function(pos, move, test=T) {
  pos_2 <- pos
  pos_2$x <- pos$x +1
  
  next_pos <- move_dir(pos, move)
  next_found <- d15_m2[next_pos$y,next_pos$x]
  
  next_pos_2 <- next_pos
  next_pos_2$x <- next_pos$x + 1
  next_found_2 <- d15_m2[next_pos_2$y,next_pos_2$x]
  
  if (next_found == "#" || next_found_2 == "#") return(F)
  
  if (next_found == d15_m2[pos$y,pos$x]) { # stacked
    out <- move_boxes(next_pos, move, test)
  } else if (next_found == "." && next_found_2 == ".") {
    out <- T
  } else {
    out <- all(
      if (next_found == "]") {
        move_boxes(move_dir(next_pos, "<"), move, test)
      } else T,
      if (next_found_2 == "[") {
        move_boxes(next_pos_2, move, test)
      } else T
    )
  }
  
  if (test==F) {
    d15_m2[pos$y,pos$x] <<- "."
    d15_m2[pos_2$y,pos_2$x] <<- "."
    d15_m2[next_pos$y,next_pos$x] <<- "["
    d15_m2[next_pos_2$y,next_pos_2$x] <<- "]"
  }
  
  return(out)
}

cur_pos <- d15_start
for (move in d15_moves) {
  next_pos <- mapply(`+`,cur_pos, get_dir(move)) |> as.list()
  next_found <- d15_m2[next_pos$y, next_pos$x] 
  if (next_found == "#") {
    next
  }
  if (next_found == ".") {
    cur_pos <- next_pos
    next
  }
  if (next_found %in% c("[", "]")) {
    if (move == ">") { # [
      next_free_space <- find_next_free_space(next_pos, move, d15_m2)
      if (!is.null(next_free_space)) {
        x = (cur_pos$x):(next_free_space$x-1)
        cur_found = d15_m2[cur_pos$y, x+1] <- d15_m2[cur_pos$y, x]
        cur_pos <- next_pos
      }
    } else if (move == "<") { # [
      next_free_space <- find_next_free_space(next_pos, move, d15_m2)
      if (!is.null(next_free_space)) {
        x = (cur_pos$x):(next_free_space$x+1)
        cur_found = d15_m2[cur_pos$y, x-1] <- d15_m2[cur_pos$y, x]
        cur_pos <- next_pos
      }
    } else {
      box_pos <- next_pos
      if (next_found == "]") box_pos$x <- next_pos$x-1
      can_move <- move_boxes(box_pos, move, T)
      if (can_move) {
        move_boxes(box_pos, move, F)
        cur_pos <- next_pos
      }
    }
  }
}

d15_2_answer <- d15_calc_gps(d15_m2)
