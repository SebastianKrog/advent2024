

input <- "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qpg
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn" |> str_split_1("\n")

input <- read_lines("data/day_23/input.txt")

df <- str_match(input, "(?<f>..)-(?<t>..)") |> 
  data.frame() |> as_tibble() |> select(f, t)

from <- df$f
to <- df$t

new_node <- function(connect) {
  out <- new.env()
  out$connects = list(connect)
  out
}

dict <- new.env()
add_node <- function(from, to) {
  if (is.null(dict[[from]])) dict[[from]] <<- new_node(to)
  else dict[[from]]$connects <<- append(dict[[from]]$connects, to)
}

for (i in 1:length(from)) {
  add_node(from[[i]], to[[i]])
  add_node(to[[i]], from[[i]])
}

uniq <- c(from, to) |> 
  unique() 

tees <- uniq |>
  str_split("") |> 
  discard(\(x) head(x, 1) != "t") |>
  map(\(x) paste(x, collapse=""))

hash_three <- function(nodes) {
  paste(sort(nodes), collapse=".")
}

find_uniq_threes <- function(tees) {
  threes <- new.env()
  
  for (node in tees) {
    for (connect in dict[[node]]$connects) {
      if (node == connect) next
      nodes = c(node, connect)
      for (three in dict[[connect]]$connects) {
        if (three %in% nodes) next
        if (node %in% dict[[three]]$connects)
          threes[[hash_three(c(three, nodes))]] <- T
      }
    }
  }
  
  names(threes)
}

answer_1 <- length(find_uniq_threes(tees))


unhash <- function(hash) {
  str_split(hash, "\\.")
}

threes <- find_uniq_threes(uniq) |> unhash()

find_next <- function(sets) {
  nnns <- new.env()
  cli::cli_progress_bar("Finding sets", total = length(uniq))
  for (node in uniq) {
    cli::cli_progress_update()
    for (set in sets) {
      if (node %in% set) next
      if (all(set %in% dict[[node]]$connects)) 
        nnns[[hash_three(c(node, set))]] <- T
    }
  }
  cli::cli_progress_done()
  
  names(nnns) |> unhash()
}

sets <- threes
while (length(sets) > 1) {
  sets <- find_next(sets)
}
print(paste(sets[[1]], collapse=","))
