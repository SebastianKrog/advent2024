library(tidyverse)

# DAY 1
d1 <- read_table("data/day_1/input.txt", col_names = c("list_1", "list_2"))

# Pair up the smallest number in the left list with the smallest number in 
# the right list, then the second-smallest left number with the 
# second-smallest right number, and so on. Within each pair, figure out 
# how far apart the two numbers are; you'll need to add up all 
# of those distances. To find the total distance between the left list and 
# the right list, add up the distances between all of the pairs you found.

d1_list_1 <- d1$list_1[order(d1$list_1)]
d1_list_2 <- d1$list_2[order(d1$list_2)]

d1_1_answer <- sum(abs(d1_list_1-d1_list_2))
d1_1_answer

# DAY 1 Task 2

# Calculate a total similarity score by adding up each number in the left 
# list after multiplying it by the number of times that number appears 
# in the right list.

d1_counts <- as.data.frame(table(d1_list_2)) %>% as.tibble() %>% 
  transmute(value = as.numeric(as.character(d1_list_2)), freq=Freq)

d1_out <- left_join(as.tibble(d1_list_1), d1_counts, by="value") %>% 
  mutate(
    freq = replace_na(freq, 0),
    score = value * freq
  )

# Finally we output the score
d1_2_answer <- sum(d1_out$score)
d1_2_answer


# DAY 2
d2_cols <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8")
d2 <- read_table("data/day_2/input.txt", col_names = d2_cols) %>% 
  mutate(
    row = row_number()
  )

# The engineers are trying to figure out which reports are safe. The 
# Red-Nosed reactor safety systems can only tolerate levels that are either
# gradually increasing or gradually decreasing. So, a report only counts as
# safe if both of the following are true:
# - The levels are either all increasing or all decreasing.
# - Any two adjacent levels differ by at least one and at most three.

d2_long <- d2 %>% gather(key="key", val="val", x1,x2,x3,x4,x5,x6,x7,x8)

find_safe <- function(df_long) {
  df_long %>% 
    arrange(row, key) %>% 
    group_by(row) %>% 
    mutate(
      diff = c(diff(val), NA),
      inc = all(diff > 0, na.rm = T),
      dec = all(diff < 0, na.rm = T),
      max_3 = all(abs(diff) <= 3, na.rm = T),
      safe = max_3 & (inc | dec)
    ) %>% ungroup()
}

d2_safe <- find_safe(d2_long)

d1_1_answer <- d2_safe %>% distinct(row, safe) %>% pull(safe) %>% sum()

# The Problem Dampener is a reactor-mounted module that lets the reactor 
# safety systems tolerate a single bad level in what would otherwise be
# a safe report.

# Update your analysis by handling situations where the Problem Dampener can
# remove a single level from unsafe reports. How many reports are now safe?

d2_unsafe <- d2_safe %>% filter(safe == F)

d2_collect <- d2_unsafe %>% select(-safe)
for (col in d2_cols) {
  safe_rows <- d2_unsafe %>% 
    filter(key != col) %>% 
    find_safe() %>% 
    distinct(row, safe) %>% 
    rename(!!paste("safe", col, sep="_") := safe)
  d2_collect <- left_join(d2_collect, safe_rows, by="row")
}

d2_new_safe <- d2_collect %>% 
  group_by(row) %>% 
  mutate(
  safe = 
    any(safe_x1, safe_x2, safe_x3, safe_x4, safe_x5, safe_x6, safe_x7, safe_x8)
) %>% distinct(row, safe)

d1_2_answer <-   d1_1_answer + d2_new_safe %>% pull(safe) %>% sum()
d1_2_answer


# DAY 3

# It seems like the goal of the program is just to multiply some numbers. 
# It does that with instructions like mul(X,Y), where X and Y are each 1-3 
# digit numbers. For instance, mul(44,46) multiplies 44 by 46 to get a result 
# of 2024. Similarly, mul(123,4) would multiply 123 by 4.

# However, because the program's memory has been corrupted, there are also 
# many invalid characters that should be ignored, even if they look like part 
# of a mul instruction. Sequences like 
# mul(4*, mul(6,9!, ?(12,34), or mul ( 2 , 4 ) do nothing.

# Scan the corrupted memory for uncorrupted mul instructions. What do you get 
# if you add up all of the results of the multiplications?

d3_file <- readLines("data/day_3/input.txt") |> paste(collapse = " ")
# Collapsed with space and not newline, since we later match to the start
# of string.

d3_regex <- "mul\\((\\d{1,3}),(\\d{1,3})\\)"

matches_to_table <- function(match_list) {
  match_list |> data.frame() %>% 
    as.tibble() |> transmute(
      fun = "mut",
      arg1 = as.numeric(X2),
      arg2 = as.numeric(X3)
    )
}

d3_matches <- str_match_all(d3_file, d3_regex) |> matches_to_table()

d3_1_answer <- d3_matches %>% summarise(sum(arg1*arg2)) %>% as.numeric()
d3_1_answer

# Here are two new instructions you'll need to handle:
# The do() instruction enables future mul instructions.
# The don't() instruction disables future mul instructions.
# Only the most recent do() or don't() instruction applies. 
# At the beginning of the program, mul instructions are enabled.

d3_dos <- str_extract_all(d3_file, "(^|do\\(\\)).+?don't\\(\\)")
d3_do_matches <- str_match_all(paste(d3_dos, collapse = ""), d3_regex) %>% 
  matches_to_table()

d3_2_answer <- d3_do_matches %>% summarise(sum(arg1*arg2)) %>% as.numeric()
d3_2_answer