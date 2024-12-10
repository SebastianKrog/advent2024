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
  ) %>% unnest(c(checksum_pos)) %>% 
  select(checksum_pos, data, id)

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

