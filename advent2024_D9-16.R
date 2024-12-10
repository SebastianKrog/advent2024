library(tidyverse)
source("utils.R")

# DAY 9

d9_input <- readLines("data/day_9/input.txt")

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

# Part 2

d9_2_df <- d9_df %>% 
  mutate(
    moved = F,
    pos = row_number()
  )

reformat_1 <- function(df) {
  ret_df <- df
  
  # Find next data to move
  next_data <- df %>% filter(data==T, moved==F) %>% filter(id == max(id))
  
  if (nrow(next_data) == 0) {
    return(list(df=ret_df, moved=F))
  }
  
  next_pos <- min(next_data$pos)
  next_count <- next_data$count[[1]]
  next_id <- next_data$id[[1]]
  
  # Find first slot to move to
  slot <- df %>% filter(data==F, pos < next_pos, count >= next_count) %>% 
    arrange(pos) %>% 
    slice_head(n=next_count)
  
  if (nrow(slot) > 0) {
    # Remove old data
    ret_df$id[next_data$pos] <- -1
    ret_df$data[next_data$pos] <- F
    
    # Add new data
    ret_df$id[slot$pos] <- next_id
    ret_df$data[slot$pos] <- T
    ret_df$moved[slot$pos] <- T
    
    # Recalculate counts
    ret_df <- ret_df %>% group_by(data, id) %>% mutate(
      count = n()
    ) %>% ungroup()
  } else {
    ret_df$moved[next_data$pos] <- T
  }
  
  return(list(df=ret_df, moved=T))
}

d9_2_out <- d9_2_df
moved <- T
cli::cli_progress_bar("Reformatting...", total=max(df$id)+1)
while(moved) {
  attempt_move <- reformat_1(d9_2_out)
  d9_2_out <- attempt_move$df
  moved <- attempt_move$moved
  cli::cli_progress_update()
}

d9_2_calc_df <- d9_2_out

d9_2_calc_df <- d9_2_calc_df %>% mutate(
  check_pos = row_number() -1,
  check_val = id*check_pos
)

d9_2_answer <- d9_2_calc_df %>% filter(data==F) %>% pull(check_val) %>% sum()
