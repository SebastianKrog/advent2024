library(tidyverse)
source("utils.R")

input <- "1
10
100
2024" |> str_split_1("\n") |> as.numeric()

input <- 123

input <- "1
2
3
2024" |> str_split_1("\n") |> as.numeric()

input <- read_lines("data/day_22/input.txt") |> as.numeric()

mix <- function(sec, val) {
  bitwXor(sec, val)
}

prune <- function(sec) {
  sec %% 16777216
}

next_sec <- function(sec) {
  sec <- prune(mix(sec, bitwShiftL(sec, 6)))
  sec <- prune(mix(sec, bitwShiftR(sec, 5)))
  #prune(mix(sec, bitwShiftL(sec, 11))) --returns some NAs because of int limits
  prune(mix(sec, (sec*2048) %% 2^31))
}

secrets <- reapply(next_sec, input, 2000)

answer_1 <- sum(secrets, na.rm = T)


# Part 2

secrets_list <- rep(NA, 1, 2000) |> as.list()
secrets_list[[1]] <- input
for (i in 2:2000) secrets_list[[i]] <- next_sec(secrets_list[[i-1]])

prices <- map(secrets_list, \(x) x %% 10) |> reduce(rbind) |> data.frame() |>
  as_tibble()

price_diffs <- prices %>% mutate_all(\(x) x - lag(x))

seq_in_v <- function(v, se) {
  idx <- which(v == se[1])
  for (i in idx) {
    if (i > length(v)-3) return(NULL)
    if ( all(v[i:(i+(length(se)-1))] == se) ) return(i+3)
  }
  NULL
}

seq_has_price <- function(prices_v, price_diffs_v, se) {
  found_seq <- seq_in_v(price_diffs_v, se)
  if (is.null(found_seq)) return(0) 
  else prices_v[[found_seq]]
}

seq_total <- function(prices, price_diffs, se) {
  sum <- 0
  for (i in 1:ncol(prices)) sum <- sum + 
      seq_has_price(prices[[i]], price_diffs[[i]], se)
  sum
}

seq_total(prices, price_diffs, se=c(-2,1,-1,3))
seq_total(prices, price_diffs, se=c(-2,-2,0,4))

exclude_seq <- function(seq) {
  if (sum(seq) >= 10) return(T)
  if (sum(seq) <= -5) return(T)
  if (sum(head(seq),3) >= 10) return(T)
  if (sum(tail(seq),3) >= 10) return(T)
  F
}

# Really slow...
max_seq <- c()
max_score <- 0
cli::cli_progress_bar("Trying sequences...", total=512)
for (h in -3:4) {
  for (i in -3:4) {
    for (j in -3:4) {
      for (k in -3:5) {
        seq = c(h, i, j, k)
        if (exclude_seq(seq)) next
        score <- seq_total(prices, price_diffs, seq)
        if (score > max_score) {
          max_seq <- seq
          max_score <- score
          cli::cli_alert_success(paste("New score", score))
        }
      }
      cli::cli_progress_update()
    }
  }
}
cli::cli_process_done()
# 2189 with sequence: "1 -3  4  4"

# New method would be counting up all possibilities into a hashmap/dict.

