library(tidyverse)
library(memoise)
source("utils.R")

input <- "x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj" |> str_split_1("\n")

input <- read_lines("data/day_24/input.txt")

split_idx <- which(input == "")

wires <- input[1:(split_idx - 1)] |> str_match_all("(...): (\\d)") |>
  map(\(x) list(wire=x[[2]], val=x[[3]]=="1"))

gates <- input[(split_idx + 1):length(input)] |> 
  str_match_all("(...) (..[RD]?) ?(...) -> (...)") |>
  map(\(x) list(in1=x[[2]], op= x[[3]], in2=x[[4]], out=x[[5]]))

output_wires <- map(gates, \(x) x$out)

all_outputs_have_signal <- function(signals = signals) {
  all(map(output_wires, \(x) !is.null(signals[[x]])) |> unlist())
}

z_wires <- str_match_all(output_wires, "^z..") |> discard(is_empty) |> unlist()

calc_gates <- function(input_wires, swap = NULL, default = FALSE) {
  if (is.null(swap)) swap <- new.env()
  signals <- new.env()
  for (el in wires) {
    signals[[el$wire]] <- default
  }
  
  for (el in input_wires) {
    signals[[el$wire]] <- el$val
  }
  
  i <- 0
  while(!all_outputs_have_signal(signals) && i < 100) {
    i <- i + 1
    for (gate in gates) {
      out <- gate$out
      if (!is.null(swap[[gate$out]])) out <- swap[[gate$out]]
      if (!is.null(signals[[out]])) next
      if (is.null(signals[[gate$in1]])) next
      if (is.null(signals[[gate$in2]])) next
      if (gate$op == "AND") signals[[out]] <-
          all(signals[[gate$in1]], signals[[gate$in2]])
      if (gate$op == "XOR") signals[[out]] <-
          xor(signals[[gate$in1]], signals[[gate$in2]])
      if (gate$op == "OR") signals[[out]] <- 
          any(signals[[gate$in1]], signals[[gate$in2]])
    }
  }
  
  if (i >= 100) return(rep(NA, 46))
  out <- z_wires |> sort() |> map(\(x) signals[[x]]) |> as.numeric()
  out
}

output_value <- function(output_wire_signals) {
  sum(2^(1:length(output_wire_signals)-1)*output_wire_signals)
}

answer_1 <- output_value(calc_gates(wires))

# Part 2

# Search space is very big (!) = 222*221*220*219*218*217*216*215

wire_name <- \(letter, num) paste(letter, str_pad(num, 2, pad="0"), sep = "")

define_input_wires <- function(x_signal, y_signal) {
  if (length(x_signal) != length(y_signal)) stop("X Y not same length") # pad 0?
  
  x_names <- map(1:length(x_signal)-1,
                 \(x) paste("x", str_pad(x, 2, pad="0"), sep = ""))
  
  y_names <- map(1:length(y_signal)-1,
                 \(x) paste("y", str_pad(x, 2, pad="0"), sep = ""))
  
  signals <- list()
  for (i in 1:length(x_signal)) {
    signals[[i*2-1]] <- list(wire = x_names[[i]], val = x_signal[[i]] == 1)
    signals[[i*2]] <- list(wire = y_names[[i]], val = y_signal[[i]] == 1)
  }
  signals
}

test_bit_pos <- function(pos, swap = NULL) {
  prepend <- rep(0, pos-1)
  all(
    calc_gates(define_input_wires(c(prepend, 0), c(prepend, 0)), swap)[[pos]] == 0,
    calc_gates(define_input_wires(c(prepend, 0), c(prepend, 1)), swap)[[pos]] == 1,
    calc_gates(define_input_wires(c(prepend, 1), c(prepend, 0)), swap)[[pos]] == 1,
    calc_gates(define_input_wires(c(prepend, 1), c(prepend, 1)), swap)[[pos]] == 0
  )
}

test_carry_pos <- function(pos, swap = NULL) {
  prepend <- rep(0, pos-1)
  all(
    calc_gates(define_input_wires(c(prepend, 1, 0), c(prepend, 1, 0)), swap)[[pos+1]] == 1,
    calc_gates(define_input_wires(c(prepend, 1, 0), c(prepend, 1, 1)), swap)[[pos+1]] == 0,
    calc_gates(define_input_wires(c(prepend, 1, 1), c(prepend, 1, 0)), swap)[[pos+1]] == 0,
    calc_gates(define_input_wires(c(prepend, 1, 1), c(prepend, 1, 1)), swap)[[pos+1]] == 1
  )
}

# We could limit the search space, but lets just not..
# length(combn(output_wires, 2)) == 49062, we might still get into trouble

make_swap <- function(x,y) {
  out <- new.env()
  out[[x]] <- y
  out[[y]] <- x
  out
}

# Let's fix bit 9

for (pair in combn(output_wires, 2, simplify = FALSE)) {
  if (isTRUE(test_bit_pos(9, make_swap(pair[[1]], pair[[2]])))) {
    print(pair)
    break
  }
  break # Well that didn't work..
}

# New solution: Look at it like a graph. What wires connect between 
# inputs x00, y00 and output z00? (For the carry, also z001)

find_gates_from_input <- memoise(function(inputs) {
  out <- c()
  for (gate in gates) {
    if (any(gate$in1 %in% inputs | gate$in2 %in% inputs)) {
      out <- c(out, list(gate), find_gates_from_input(gate$out))
    }
  }
  unique(out)
})
  

find_gates_from_output <- memoise(function(outputs) {
  out <- c()
  for (gate in gates) {
    if (any(gate$out %in% outputs)) {
      out <- c(out, list(gate), find_gates_from_output(c(gate$in1, gate$in2)))
    }
  }
  unique(out)
})

find_bit_gates <- function(pos) {
  input_pos_gates <- find_gates_from_input(wire_name(c("x", "y"), pos-1))
  input_p1_gates <- find_gates_from_input(wire_name(c("x", "y"), pos))
  input_gates <- input_pos_gates[which(!input_pos_gates %in% input_p1_gates)]
  
  output_pos_gates <- find_gates_from_output(wire_name("z", pos-1))
  output_m1_gates <- find_gates_from_output(wire_name("z", pos-2))
  output_gates <- output_pos_gates[which(!output_pos_gates %in% output_m1_gates)]
  input_gates
}

# Test connections?


correct_out <- new.env()
for (wire in c("z00", "rhk", "gjw", "z45")) {
  correct_out[[wire]] = T
}
input_pos <- 45
test_structure <- memoise(function(input_pos) {
  if (input_pos == 1) return("rhk")
  input_wires <- wire_name(c("x", "y"), input_pos-1)
  
  layer_1_gates <- gates[which(unlist(map(gates, \(x) x$in1 %in% input_wires && x$in2 %in% input_wires)))]
  layer_1_XOR <- first(layer_1_gates[which(unlist(map(layer_1_gates, \(x) x$op == "XOR")))])
  layer_1_AND <- first(layer_1_gates[which(unlist(map(layer_1_gates, \(x) x$op == "AND")))])
  
  layer_1_XOR_layer_2 <- gates[which(unlist(map(gates, \(x) layer_1_XOR$out %in% c(x$in1, x$in2))))]
  if (length(layer_1_XOR_layer_2) != 2) return(NULL)
  layer_1_XOR_layer_2_XOR <- first(layer_1_XOR_layer_2[which(unlist(map(layer_1_XOR_layer_2, \(x) x$op == "XOR")))])
  layer_1_XOR_layer_2_AND <- first(layer_1_XOR_layer_2[which(unlist(map(layer_1_XOR_layer_2, \(x) x$op == "AND")))])
  
  if (layer_1_XOR_layer_2_XOR$out == wire_name("z", input_pos-1)) {
    correct_out[[layer_1_XOR$out]] <<- T
    correct_out[[layer_1_XOR_layer_2_XOR$out]] <<- T
  }
  
  carry_in <- c(layer_1_XOR_layer_2_XOR$in1, layer_1_XOR_layer_2_XOR$in2)[which(c(layer_1_XOR_layer_2_XOR$in1, layer_1_XOR_layer_2_XOR$in2) != layer_1_XOR$out)]
  carry_in2 <- c(layer_1_XOR_layer_2_AND$in1, layer_1_XOR_layer_2_AND$in2)[which(c(layer_1_XOR_layer_2_AND$in1, layer_1_XOR_layer_2_AND$in2) != layer_1_XOR$out)]
  if (!(carry_in == carry_in2)) return(NULL)
  
  test_minus_one <- test_structure(input_pos-1)
  if (!is.null(test_minus_one) && carry_in == test_minus_one) {
    # Now we know the AND is correct and the carry_in
    correct_out[[layer_1_XOR_layer_2_AND$out]] <<- T
    correct_out[[carry_in]] <<- T
  }
  
  or_gate_inputs <- c(layer_1_AND$out, layer_1_XOR_layer_2_AND$out)
  or_gate <- first(gates[which(unlist(map(gates, \(x) all(c(x$in1, x$in2) %in% or_gate_inputs))))])
  
  if (!is.null(or_gate) && or_gate$op == "OR") {
    correct_out[[layer_1_AND$out]] <<- T
  }
  
  or_gate$out
})

for (i in 1:45) {
  test_structure(i)
}

possibly_faulty <- output_wires[which(!output_wires %in% names(correct_out))] |> unlist()

# "wss" <=> "wrm" -- fixes 15?

# Let's fix bit 15
for (pair in combn(possibly_faulty, 2, simplify = FALSE)) {
  if (isTRUE(test_bit_pos(15, make_swap(pair[[1]], pair[[2]])))) {
    print(pair)
  }
  # [1] "mjj" "z14"
  # [1] "z14" "wrm"
  # [1] "wss" "wrm"
}

# And 23?
for (pair in combn(possibly_faulty, 2, simplify = FALSE)) {
  if (isTRUE(test_bit_pos(23, make_swap(pair[[1]], pair[[2]])))) {
    print(pair)
  }
  # [1] "fjs" "z22"
  # [1] "z22" "cdf"
  # [1] "z22" "hwq"
}

# And 30?
for (pair in combn(possibly_faulty, 2, simplify = FALSE)) {
  if (isTRUE(test_bit_pos(30, make_swap(pair[[1]], pair[[2]])))) {
    print(pair)
  }
  # "gbs" "z29"
}

make_swaps <- function(l) {
  out <- new.env()
  for (el in l) {
    out[[el[[2]]]] <- el[[1]]
    out[[el[[1]]]] <- el[[2]]
  }
  out
}

calc_gates(
  define_input_wires(c(1, rep(0, 44)), rep(0, 45)), 
  make_swaps(list(c("gbs","z29"), c("wss","wrm"))))

# I think it's one of the three..
calc_gates(
  define_input_wires(c(1, rep(0, 44)), rep(1, 45)), 
  make_swaps(list(c("gbs","z29"), c("wss","wrm"), c("fjs","z22"))))

calc_gates(
  define_input_wires(c(1, rep(0, 44)), rep(1, 45)), 
  make_swaps(list(c("gbs","z29"), c("wss","wrm"), c("z22","cdf"))))

calc_gates(
  define_input_wires(c(1, rep(0, 44)), rep(1, 45)), 
  make_swaps(list(c("gbs","z29"), c("wss","wrm"), c("z22","hwq")))) == c(rep(0, 45), 1)

# And 9???
test_wires <- output_wires[!output_wires %in% c("gbs","z29","wss","wrm","z22")]
combs <- combn(test_wires, 2, simplify = FALSE)
cli::cli_progress_bar("Testing...", total=length(combs))
for (pair in combs) {
  if(any(
    identical(calc_gates(
      define_input_wires(c(1, rep(0, 44)), rep(1, 45)), 
      make_swaps(list(c(pair[[1]], pair[[2]]), c("gbs","z29"), c("wss","wrm"), c("z22","fjs")))), c(rep(0, 45), 1)),
    identical(calc_gates(
      define_input_wires(c(1, rep(0, 44)), rep(1, 45)), 
      make_swaps(list(c(pair[[1]], pair[[2]]), c("gbs","z29"), c("wss","wrm"), c("z22","cdf")))), c(rep(0, 45), 1)),
    identical(calc_gates(
      define_input_wires(c(1, rep(0, 44)), rep(1, 45)), 
      make_swaps(list(c(pair[[1]], pair[[2]]), c("gbs","z29"), c("wss","wrm"), c("z22","hwq")))), c(rep(0, 45), 1))
  )) print(pair)
  cli::cli_progress_update()
}

calc_gates(define_input_wires(c(1, rep(0, 44)), rep(1, 45)), 
make_swaps(list(c("smb", "qth"), 
                c("gbs","z29"), c("wss","wrm"), c("z22","hwq"))))

all(calc_gates(define_input_wires(c(1, rep(0:1, 22)), c(1, rep(0:1, 22))), 
           make_swaps(list(c("smb", "qth"), 
                           c("gbs","z29"), c("wss","wrm"), c("z22","hwq")))) ==
          rep(0:1, 23))

all(calc_gates(define_input_wires(c(0, rep(1:0, 22)), c(0, rep(1:0, 22))), 
               make_swaps(list(c("smb", "qth"), 
                               c("gbs","z29"), c("wss","wrm"), c("z22","hwq")))) ==
      c(0, 0, rep(1:0, 22)))

# So the above code found the following possible fixes
tmp_list <- c("smb", "qth", "smb", "frr", "smb", "vqp", "bdr", "vqp", "qth", "rhg", "qth", "vkh", "qth", "qcj", "qth", "cmd", "qth", "fpg", "qth", "pqp", "qth", "ncj", "qth", "qdj", "qth", "cvf", "qth", "ctv", "qth", "gsm", "qth", "mfj", "qth", "mqv", "qth", "csw", "qth", "dgr", "qth", "gwq", "qth", "tmm", "qth", "wjm", "qth", "hpp", "qth", "frc", "qth", "tnm", "qth", "jth", "qth", "jsb", "qth", "qbb", "qth", "qwd", "qth", "gng", "qth", "sng", "qth", "ksh", "qth", "mpr", "qth", "vgw", "qth", "jrk", "qth", "ptf", "qth", "ghm", "qth", "grd", "rhg", "frr", "rhg", "vqp", "vkh", "frr", "vkh", "vqp", "qcj", "frr", "qcj", "vqp", "hct", "vqp", "cmd", "frr", "cmd", "vqp", "fpg", "frr", "fpg", "vqp", "chb", "vqp", "pqp", "frr", "pqp", "vqp", "ncj", "frr", "ncj", "vqp", "frr", "qdj", "frr", "cvf", "frr", "ctv", "frr", "gsm", "frr", "mfj", "frr", "mqv", "frr", "csw", "frr", "dgr", "frr", "gwq", "frr", "tmm", "frr", "wjm", "frr", "hpp", "frr", "frc", "frr", "tnm", "frr", "jth", "frr", "jsb", "frr", "qbb", "frr", "qwd", "frr", "gng", "frr", "sng", "frr", "ksh", "frr", "mpr", "frr", "vgw", "frr", "jrk", "frr", "ptf", "frr", "ghm", "frr", "grd", "qdj", "vqp", "cvf", "vqp", "ctv", "vqp", "gsm", "vqp", "z08", "dnc", "z08", "thm", "z08", "mpr", "mfj", "vqp", "mqv", "vqp", "csw", "vqp", "dgr", "vqp", "gwq", "vqp", "tmm", "vqp", "wjm", "vqp", "hpp", "vqp", "sjw", "vqp", "frc", "vqp", "tnm", "vqp", "ggw", "vqp", "bkg", "vqp", "jth", "vqp", "jsb", "vqp", "qbb", "vqp", "qwd", "vqp", "gng", "vqp", "sng", "vqp", "ksh", "vqp", "mpr", "vqp", "dhh", "vqp", "vgw", "vqp", "jrk", "vqp", "ptf", "vqp", "ghm", "vqp", "vqp", "grd") |> unique()

# My test case wasn't good enough
combs <- combn(tmp_list, 2, simplify = FALSE)
cli::cli_progress_bar("Testing...", total=length(combs))
for (pair in combs) {
  if(all(
    identical(calc_gates(
      define_input_wires(c(1, rep(0, 44)), rep(1, 45)), 
      make_swaps(list(c(pair[[1]], pair[[2]]), c("gbs","z29"), c("wss","wrm"), c("z22","fjs")))), c(rep(0, 45), 1)),
    all(calc_gates(define_input_wires(c(1, rep(0:1, 22)), c(1, rep(0:1, 22))), 
                   make_swaps(list(c(pair[[1]], pair[[2]]), 
                                   c("gbs","z29"), c("wss","wrm"), c("z22","fjs")))) ==
        rep(0:1, 23)),
    all(calc_gates(define_input_wires(c(0, rep(1:0, 22)), c(0, rep(1:0, 22))), 
                   make_swaps(list(c(pair[[1]], pair[[2]]), 
                                   c("gbs","z29"), c("wss","wrm"), c("z22","fjs")))) ==
        c(0, 0, rep(1:0, 22)))
      )) print(pair)
  if(all(
    identical(calc_gates(
      define_input_wires(c(1, rep(0, 44)), rep(1, 45)), 
      make_swaps(list(c(pair[[1]], pair[[2]]), c("gbs","z29"), c("wss","wrm"), c("z22","cdf")))), c(rep(0, 45), 1)),
    all(calc_gates(define_input_wires(c(1, rep(0:1, 22)), c(1, rep(0:1, 22))), 
                   make_swaps(list(c(pair[[1]], pair[[2]]), 
                                   c("gbs","z29"), c("wss","wrm"), c("z22","cdf")))) ==
        rep(0:1, 23)),
    all(calc_gates(define_input_wires(c(0, rep(1:0, 22)), c(0, rep(1:0, 22))), 
                   make_swaps(list(c(pair[[1]], pair[[2]]), 
                                   c("gbs","z29"), c("wss","wrm"), c("z22","cdf")))) ==
        c(0, 0, rep(1:0, 22)))
    )) print(pair)
  if(all(
    identical(calc_gates(
      define_input_wires(c(1, rep(0, 44)), rep(1, 45)), 
      make_swaps(list(c(pair[[1]], pair[[2]]), c("gbs","z29"), c("wss","wrm"), c("z22","hwq")))), c(rep(0, 45), 1)),
   all(calc_gates(define_input_wires(c(1, rep(0:1, 22)), c(1, rep(0:1, 22))), 
                   make_swaps(list(c(pair[[1]], pair[[2]]), 
                                   c("gbs","z29"), c("wss","wrm"), c("z22","hwq")))) ==
        rep(0:1, 23)),
    all(calc_gates(define_input_wires(c(0, rep(1:0, 22)), c(0, rep(1:0, 22))), 
                   make_swaps(list(c(pair[[1]], pair[[2]]), 
                                   c("gbs","z29"), c("wss","wrm"), c("z22","hwq")))) ==
        c(0, 0, rep(1:0, 22)))
    )) print(pair)
  cli::cli_progress_update()
}

# Found this: "z08" "thm"

pair <- c("z08","thm")
if(all(
  identical(calc_gates(
    define_input_wires(c(1, rep(0, 44)), rep(1, 45)), 
    make_swaps(list(c(pair[[1]], pair[[2]]), c("gbs","z29"), c("wss","wrm"), c("z22","hwq")))), c(rep(0, 45), 1)),
  all(calc_gates(define_input_wires(c(1, rep(0:1, 22)), c(1, rep(0:1, 22))), 
                 make_swaps(list(c(pair[[1]], pair[[2]]), 
                                 c("gbs","z29"), c("wss","wrm"), c("z22","hwq")))) ==
      rep(0:1, 23)),
  all(calc_gates(define_input_wires(c(0, rep(1:0, 22)), c(0, rep(1:0, 22))), 
                 make_swaps(list(c(pair[[1]], pair[[2]]), 
                                 c("gbs","z29"), c("wss","wrm"), c("z22","hwq")))) ==
      c(0, 0, rep(1:0, 22)))
)) print(paste(1, pair))

swaps <- c("z08", "thm", "gbs","z29","wss","wrm","z22","hwq") |> sort()
paste(swaps, collapse=",")
#gbs,hwq,thm,wrm,wss,z08,z22,z29
