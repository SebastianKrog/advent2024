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
run_program <- function(a, test=T) {
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
  if (!test) return(output)
  if (identical(output, program)) return(T) else return(F)
}

# 5
out2 <- function(combo) {
  if (length(output) == length(program)) return(T)
  new_out <- read_combo(combo) %% 8
  output <<- c(output, new_out)
  return(!identical(new_out, program[[length(output)]]))
}

x <- 27278262
y <- x
while (y < x*10) {
  if (run_program(y)) {
    answer_2 <- y
    break
  } else y <- y + 1
}

answer_2



x <- 27278262
y <- x - 1
while (y < x*10) {
  y <- y + 1
  if(first_out_one(i) != 1) next
  A <- y
  B <- 0
  C <- 0
  pointer <<- 0
  output <<- c()
  i <- 0
  while(i < 1000) {
    index <- pointer + 1
    if (index > length(program)-1) break
    operand <- program[index+1]
    opcode <- program[index]
    if (opcode == 0) A <- trunc(A/(2^read_combo(operand)))
    if (opcode == 1) B <- bitwXor(B, operand)
    if (opcode == 2) B <- read_combo(operand) %% 8
    if (opcode == 3 && A != 0) pointer <<- operand - 2
    if (opcode == 4) B <- bitwXor(B, C)
    if (opcode == 5 && out2(operand)) break
    if (opcode == 6) B <- trunc(A/(2^read_combo(operand)))
    if (opcode == 7) C <- trunc(A/(2^read_combo(operand)))
    pointer <- pointer + 2
    i <- i + 1
  }
  if (identical(output, program)) {
    answer_2 <- y
    break
  }
}

# Calc if first output == 2
A <- x
B <- 0
C <- 0
program <- c(2, 4, 1, 2, 7, 5, 4, 7, 1, 3, 5, 5, 0, 3, 3, 0)
# 2 4
B <- A %% 8
# 1 2
B <- bitwXor(B, 2)
# 7 5
C <- bitwShiftR(A,B) = trunc(A/(2^B))
# 4 7
B <- bitwXor(B, C)
# 1 3
B <- bitwXor(B, 3)
# 5 5
output B
# 0 3
A <- bitwShiftR(A,3)
# 3 0
goto start unless A = 0

# Rewrite
first_out <- function(a) {
  bitwXor(bitwXor(bitwXor(a %% 8, 2), bitwShiftR(a, bitwXor(a %% 8, 2))), 3)
}

find_bits <- function(n, x) {
  for (a in (0:2^6)+x) {
    if (bitwXor(bitwXor(bitwXor(a %% 8, 2), bitwShiftR(a, bitwXor(a %% 8, 2))), 3) == n) return(a)
  }
  stop("Huh")
}

find_a <- function(program) {
  output <- rev(program)
  collect <- c()
  x <- 0
  for (i in 1:length(output)) {
    next_out <- output[[i]]
    num <- find_bits(next_out, x)
    x <- bitwShiftR(num, 3)
    collect <- c(num, collect)
  }
  collect
}

#first_out_one <- function(a) {
#  b <- a %% 8
#  bitwXor(bitwXor(b, 2), trunc(a/(2^bitwXor(b, 2))))
#}

i <- 0
n <- 10000000
found <- 0
while (i<n) {
  if(first_out_one(i) == 1) {
    found <- found + 1
    break
  }
  i <- i + 1
}
found/n



# Reverse build the output.
A = 0
B = 0



# Test bits
bit_set <- c(2, 4, 1, 2, 7, 5, 4, 7, 1, 3, 5, 5, 0, 3, 3, 0)
calc_bits <- 2^(3*(1:(length(bit_set))-1))*bit_set


# LETS TRY AGAIN

# Not for too big numbers...
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

find_a <- function(program) {
  output <- rev(program)
  a <- c()
  for (o in output) {
    for (n in 0:7) {
      a_new <- head(c(bit_list(n), 0, 0), 3)
      a_test <- c(a_new, a)
      if (head(run_program_bl(a_test), 1) == o) {
        a <- a_test
        break
      }
      stop("NOT FOUND")
    }
    print(a)
    print(run_program_bl(a))
    print(" ")
  }
  a
}

find_A <- function(program) {
  bitly <- \(n) head(c(bit_list(n), 0, 0), 3)
  z_s <- lapply(0:7, bitly)
  
  .test <- function(a, test_out) {
    l <- z_s |> lapply(\(x) c(x, a))
    ll <- lapply(l, \(a_test) first(run_program_bl(a_test)) == test_out) |> unlist()
    (l)[which(ll)]
  }
  
  .find_A <- function(output, A, envi = parent.frame()) {
    As <- .test(A, first(output))
    if (length(As) == 0) return()
    if (length(output) == 1) { do.call("return", list(first(As)), envir = envi) }
    walk(As, function(a) { .find_A(tail(output, -1), a, envi) })
  }
  
  .find_A(rev(program), c())
}
