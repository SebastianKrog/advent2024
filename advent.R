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

d2_1_answer <- d2_safe %>% distinct(row, safe) %>% pull(safe) %>% sum()
d2_1_answer

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

d2_2_answer <-d2_1_answer + d2_new_safe %>% pull(safe) %>% sum()
d2_2_answer


# DAY 3

d3_file <- readLines("data/day_3/input.txt") |> paste(collapse = " ")
# Collapsed with space and not newline, since we later match to the start
# of string.

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

d3_regex <- "mul\\((\\d{1,3}),(\\d{1,3})\\)"

matches_to_table <- function(match_list) {
  match_list |> data.frame() %>% 
    as.tibble() |> transmute(
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


# DAY 4

d4 <- readLines("data/day_4/input.txt") |> paste(collapse=" ")

# As the search for the Chief continues, a small Elf who lives on the station 
# tugs on your shirt; she'd like to know if you could help her with her word
# search (your puzzle input). She only has to find one word: XMAS.

# This word search allows words to be horizontal, vertical, diagonal, written 
# backwards, or even overlapping other words. It's a little unusual, though, as
# you don't merely need to find one instance of XMAS - you need to find all 
# of them.

count_matches <- function(string, regex) {
  str_extract_all(string, regex)[[1]] |> length()
}

combine_regex <- function(...) { paste(..., sep="|") }

d4_horizontal <- count_matches(d4, combine_regex("X(?=MAS)", "S(?=AMX)"))

# Magic number of 140 = line length.
# E.g. readLines("data/day_4/input.txt")[[1]] |> nchar()
d4_vertical <- count_matches(d4, combine_regex(
  "X(?=.{140}M.{140}A.{140}S)",
  "S(?=.{140}A.{140}M.{140}X)"
))

d4_vert_right <- count_matches(d4, combine_regex(
  "X(?=.{141}M.{141}A.{141}S)",
  "S(?=.{141}A.{141}M.{141}X)"
))

d4_vert_left <- count_matches(d4, combine_regex(
  "X(?=.{139}M.{139}A.{139}S)",
  "S(?=.{139}A.{139}M.{139}X)"
))

d4_1_answer <- sum(d4_horizontal, d4_vertical, d4_vert_right, d4_vert_left)
d4_1_answer

# Looking for the instructions, you flip over the word search to find that this 
# isn't actually an XMAS puzzle; it's an X-MAS puzzle in which you're supposed 
# to find two MAS in the shape of an X.

d4_2_answer <- count_matches(d4, combine_regex(
  "M(?=.(M.{139}A.{139}S|S.{139}A.{139}M).S)",
  "S(?=.(M.{139}A.{139}S|S.{139}A.{139}M).M)"
))
d4_2_answer


# DAY5

d5 <- readLines("data/day_5/input.txt")

# Safety protocols clearly indicate that new pages for the safety manuals must
# be printed in a very specific order. The notation X|Y means that if both page
# number X and page number Y are to be produced as part of an update, page
# number X must be printed at some point before page number Y.

# The Elf has for you both the page ordering rules and the pages to produce in
# each update (your puzzle input), but can't figure out whether each update has
# the pages in the right order.

d5_split <- which(d5 == "")
d5_page_rules_list <- d5[1:(d5_split-1)]

d5_page_rules <- hashtab()
add_page_rule <- function(page_rule_str) {
  pr <- as.numeric(str_match(page_rule_str,"(\\d+)\\|(\\d+)")[,2:3])
  d5_page_rules[[pr[2]]] <- unique(c(d5_page_rules[[pr[2], nomatch=c()]], pr[1]))
}

walk(d5_page_rules_list, add_page_rule)

d5_updates <- lapply(d5[(d5_split+1):length(d5)] |> strsplit(","), as.numeric)

check_update_allowed <- function(update) {
  .check_update_allowed <- function(upd) {
    if(length(upd) <= 1) { return(T) }
    car <- head(upd, 1)
    cdr <- tail(upd, -1)
    all(
      !(d5_page_rules[[car, nomatch=c()]] %in% cdr),
      .check_update_allowed(cdr)
    )
  }
  if (.check_update_allowed(update)) { 
    return(update[[ceiling(length(update)/2)]]) 
  }
  return(0)
}

d5_allowed_updates <- sapply(d5_updates, check_update_allowed)

d5_1_answer <- sum(d5_allowed_updates)

# For each of the incorrectly-ordered updates, use the page ordering rules to
# put the page numbers in the right order. 

# Find the updates which are not in the correct order. What do you get if you
# add up the middle page numbers after correctly ordering just those updates?

d5_incorrect <- d5_updates[which(d5_allowed_updates==0)]

fix_incorrect_update <- function(update) {
  .find_correct_element <- function(upd) {
    if(length(upd) == 1) { return(upd) }
    
    for (el in upd) {
      illegal <- d5_page_rules[[el, nomatch=c()]] %in% upd
      if (!any(illegal)) { return( el )}
    }
  }
   
  order <- c()
  subset <- update
  for (i in 1:length(update)) {
    next_el <- .find_correct_element(subset)
    order <- c(order, next_el)
    subset <- subset[! subset %in% next_el]
  }
  return(order[[ceiling(length(order)/2)]]) 
}

d5_2_answer <- sum(sapply(d5_incorrect, fix_incorrect_update))


# Day 6


# DAY 7

d7 <- readLines("data/day_7/input.txt")

# Each line represents a single equation. The test value appears before the
# colon on each line; it is your job to determine whether the remaining numbers
# can be combined with operators to produce the test value.

# Operators are always evaluated left-to-right, not according to precedence
# rules. Furthermore, numbers in the equations cannot be rearranged. Glancing
# into the jungle, you can see elephants holding two different types of
# operators: add (+) and multiply (*).

# Determine which equations could possibly be true. What is their total
# calibration result?

test_line <- d7[[1]]

d7_eqs <- str_match_all(d7 |> paste(collapse="\n"),
                        "(?<testValue>\\d+): (?<numbers>(\\d+ ?)*)")

d7_test_values <- as.numeric(d7_eqs[[1]][,2])

d7_numbers <- d7_eqs[[1]][,3]
d7_numbers <- lapply(d7_numbers, function(v) { 
  as.numeric(str_split(v, " ")[[1]]) } )

find_equation <- function(test_value, numbers) {
  .find_eq <- function(cur_val, nums) {
    if ( length(nums) == 0 ) {
      return(cur_val == test_value)
    }
    
    if ( cur_val > test_value ) { return(F) }
    
    any(
      .find_eq(cur_val + head(nums, 1), tail(nums, -1)),
      .find_eq(cur_val * head(nums, 1), tail(nums, -1)),
    )
  }
  .find_eq(0, numbers)
}

# Warning, long run time ~20 sec
d7_valid <- mapply(find_equation, d7_test_values, d7_numbers)

d7_1_answer <- sum(d7_test_values[which(d7_valid)])

# The concatenation operator (||) combines the digits from its left and right
# inputs into a single number. For example, 12 || 345 would become 12345.
# All operators are still evaluated left-to-right.

concat_numbers <- function(numbers) {
  as.numeric(paste(as.character(numbers), collapse = ""))
}

find_equation2 <- function(test_value, numbers) {
  .find_eq <- function(nums) {
    if ( length(nums) == 1 ) {
      return(nums == test_value)
    }
    
    if ( sum(nums) > test_value + 1 ) { return(F) }
    
    any(
      .find_eq(c(sum(head(nums, 2)), tail(nums, -2))),
      .find_eq(c(prod(head(nums, 2)), tail(nums, -2))),
      .find_eq(c(concat_numbers(head(nums, 2)), tail(nums, -2)))
    )
  }
  
  .find_eq(numbers)
}

# Warning, LONG run time (couple of minutes on laptop)
d7_2_valid <- mapply(find_equation2, d7_test_values, d7_numbers)

d7_2_answer <- sum(d7_test_values[which(d7_2_valid)])
