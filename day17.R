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
  if (opcode == 0) adv(operand)
  if (opcode == 1) bxl(operand)
  if (opcode == 2) bst(operand)
  if (opcode == 3) jnz(operand)
  if (opcode == 4) bxc(operand)
  if (opcode == 5) out(operand)
  if (opcode == 6) bdv(operand)
  if (opcode == 7) cdv(operand)
  pointer <<- pointer + 2
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
adv <- function(combo) A <<- trunc(A/(2^read_combo(combo)))

# 1
bxl <- function(literal) B <<- bitwXor(B, literal)

# 2
bst <- function(combo) B <<- read_combo(combo) %% 8

# 3
jnz <- function(literal) if (A != 0) pointer <<- literal - 2

# 4
bxc <- function(ignore) B <<- bitwXor(B, C)

# 5
out <- function(combo) output <<- c(output, read_combo(combo) %% 8)

# 6
bdv <- function(combo) B <<- trunc(A/(2^read_combo(combo)))

# 7
cdv <- function(combo) C <<- trunc(A/(2^read_combo(combo)))

# # First solution
# i <- 0
# while(read_op()) i <- i + 1
# answer_1 <- paste0(output, collapse=",")
# answer_1


## PART 2

# Had to completely redesign my approach since R does not support ints > 2^31
# R really let me down here...
# Hardcoded the program that was given and used a custom bitvector datatype

bit_list <- function(int) {
  if (int == 0) return(c(0))
  out <- c()
  while(int > 0) {
    out <- c(out, int %% 2)
    int <- trunc(int/2)
  }
  out
}

bl2num <- function(bit_list) {
  sum(2^(1:(length(bit_list))-1)*bit_list)
}

run_program_bl <- function(a) {
  # Hardcoded version of the program given
  output <- c()
  while(length(a) > 0) {
    tmp <- c(a, 0, 0)
    b <- head(tmp, 3)
    b <- bitwXor(b, c(0, 1, 0))
    tmp_b <- bl2num(b)
    if (tmp_b == 0) c <- head(tmp, 3)
    else if (length(a) < tmp_b) c <- c(0, 0, 0)
    else c <- head(tail(tmp, -tmp_b), 3)
    b <- bitwXor(b, c)
    b <- bitwXor(b, c(1, 1, 0))
    output <- c(output, bl2num(b))
    a <- tail(a, -3)
  }
  output
}

answer_1 <- run_program_bl(bit_list(input_registers[[1]])) |> 
  paste(collapse=",")
print(answer_1)

find_A <- function(program) {
  z_s <- lapply(0:7, \(n) head(c(bit_list(n), 0, 0), 3))
  
  .test <- function(a, test_out) {
    # Prepend all copies of 0-7 (3 bits)
    l <- z_s |> lapply(\(x) c(x, a)) 
    
    # Test them against the next output
    ll <- lapply(l, \(a_test) first(run_program_bl(a_test)) == test_out) |> 
      unlist() 
    
    # Returns all the bit sets that give the correct output
    (l)[which(ll)]
  }
  
  .find_A <- function(output, A, envi = parent.frame()) {
    # Test all next variations
    As <- .test(A, first(output))
    
    # Exit this node early if we can't continue
    if (length(As) == 0) return()
    
    # Exit fully from the find_A function if we found the result
    if (length(output) == 1) return_from(first(As), envi)
    
    # Or keep going (prioritizing the lowest A found)
    walk(As, function(a) { .find_A(tail(output, -1), a, envi) })
  }
  
  .find_A(rev(program), c())
}

answer_2 <- bl2num(find_A(program))
print(format(answer_2, scientific = F))
