library(tidyverse)
source("utils.R")

input <- "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0" |> str_split_1("\n")

input <- "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0" |> str_split_1("\n")

input <- read_lines("data/day_17/input.txt")

input_registers <- str_match_all(input[1:3], ": (\\d+)") |> 
  lapply(\(x) as.numeric(x[[2]])) |> unlist()

A <- input_registers[[1]]
B <- input_registers[[2]]
C <- input_registers[[3]]

pointer <- 0

output <- c()

program <- str_split_1(str_split_1(input[[5]], " ")[[2]], ",") |> as.numeric()

read_op <- function() {
  index <- pointer + 1
  if (index > length(program)-1) return(F)
  operand <- program[index+1]
  opcode <- program[index]
  s <- T
  if (opcode == 0) adv(operand)
  if (opcode == 1) bxl(operand)
  if (opcode == 2) bst(operand)
  if (opcode == 3) jnz(operand)
  if (opcode == 4) bxc(operand)
  if (opcode == 5) s <- out(operand)
  if (opcode == 6) bdv(operand)
  if (opcode == 7) cdv(operand)
  pointer <<- pointer + 2
  if (s == F) return(F)
  return(T)
}

read_combo <- function(combo) {
  if (combo %in% 0:3) return(combo)
  if (combo == 4) return(A)
  if (combo == 5) return(B)
  if (combo == 6) return(C)
  if (combo == 7) stop("Should not occur!")
}

# 0
adv <- function(combo) {
  A <<- trunc(A/(2^read_combo(combo)))
}

# 1
bxl <- function(literal) {
  B <<- bitwXor(B, literal)
}

# 2
bst <- function(combo) {
  B <<- read_combo(combo) %% 8
}

# 3
jnz <- function(literal) {
  if (A != 0) pointer <<- literal - 2
}

# 4
bxc <- function(ignore) {
  B <<- bitwXor(B, C)
}

# 5
out <- function(combo) {
  output <<- c(output, read_combo(combo) %% 8)
  T
}

# 6
bdv <- function(combo) {
  B <<- trunc(A/(2^read_combo(combo)))
}

# 7
cdv <- function(combo) {
  C <<- trunc(A/(2^read_combo(combo)))
}

i <- 0
while(read_op()) i <- i + 1
answer_1 <- paste0(output, collapse=",")
answer_1


## PART 2
run_program <- function(a) {
  A <<- a
  B <<- 0
  C <<- 0
  pointer <<- 0
  output <<- c()
  i <- 0
  while(i < 1000) {
    if (!read_op()) break
    i <- i + 1
  }
  if (identical(output, program)) return(T) else return(F)
}

# 5
out <- function(combo) {
  output <<- c(output, read_combo(combo) %% 8)
  return(identical(output, head(program, length(output))))
}

x <- 10768062
y <- x
while (y < x*10) {
  if (run_program(y)) {
    answer_2 <- y
    break
  } else y <- y + 1
}

answer_2
