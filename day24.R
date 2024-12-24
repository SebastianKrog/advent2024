

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

all_outputs_have_signal <- function() {
  all(map(output_wires, \(x) !is.null(signals[[x]])) |> unlist())
}

signals <- new.env()
for (el in wires) {
  signals[[el$wire]] <- el$val
}

i <- 0
while(!all_outputs_have_signal()) {
  i <- i + 1
  for (gate in gates) {
    if (is.null(signals[[gate$in1]])) next
    if (is.null(signals[[gate$in2]])) next
    if (gate$op == "AND") signals[[gate$out]] <-
        all(signals[[gate$in1]], signals[[gate$in2]])
    if (gate$op == "XOR") signals[[gate$out]] <-
      xor(signals[[gate$in1]], signals[[gate$in2]])
    if (gate$op == "OR") signals[[gate$out]] <- 
        any(signals[[gate$in1]], signals[[gate$in2]])
  }
}
print(i)

z_wires <- str_match_all(output_wires, "^z..") |> discard(is_empty) |> unlist()

z_wire_values <- z_wires |> sort() |> map(\(x) signals[[x]]) |> as.numeric()

answer_1 <- sum(2^(1:length(z_wire_values)-1)*z_wire_values)

# Part 2

# Search space is very big (!) = 222*221*220*219*218*217*216*215
