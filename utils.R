
# Read a string symbol map into x-y cooordinates
read_map_df <- function(input, ignore=".", fun=identity) {
  max_size <- nchar(input[1])*length(input)
  l_x <- rep(NA, max_size)
  l_y <- rep(NA, max_size)
  l_symbol <- rep(NA, max_size)
  
  i <- 1
  for (y in 1:length(input)) {
    l_string <- stringr::str_split_1(input[[y]], "")
    for (x in 1:length(l_string)) {
      if (!l_string[[x]] %in% ignore) {
        l_x[[i]] <- x
        l_y[[i]] <- y
        l_symbol[[i]] <- fun(l_string[[x]])
        i <- i + 1
      }
    }
  }
  
  l_x <- l_x[!is.na(l_x)]
  l_y <- l_y[!is.na(l_y)]
  l_symbol <- l_symbol[!is.na(l_symbol)]

  tibble(x=l_x, y=l_y, symbol=l_symbol)
}

read_map_matrix <- function(input, fun=identity) {
  do.call(rbind, lapply(strsplit(input, ""), fun))
}

print_map_df <- function(
  df, symbol="symbol", background=".",
  width=max(df$x),
  height=max(df$y),
  count=F,
  base_zero=F
) {
  if (base_zero) one_less <- 1 else one_less <- 0
  for (y_i in 1:height) {
    l <- rep(background, width)
    for (x_i in 1:width) {
      found <- filter(df, x==x_i-one_less, y==y_i-one_less)
      if (nrow(found) > 0) {
        if (count) l[[x_i]] <- as.character(nrow(found))
        else l[[x_i]] <- head(as.character(pull(found, symbol)), 1)
      }
    }
    cat(paste(l, collapse=""), "\n")
  }
}

map_matrix_to_df <- function(mat, ignore=".", fun=identity) {
  max_size <- prod(dim(mat))
  l_x <- rep(NA, max_size)
  l_y <- rep(NA, max_size)
  l_symbol <- rep(NA, max_size)
  
  i <- 1
  for (y in 1:nrow(mat)) {
    for (x in 1:ncol(mat)) {
      if (!mat[y,x] %in% ignore) {
        l_x[[i]] <- x
        l_y[[i]] <- y
        l_symbol[[i]] <- fun(mat[y,x])
        i <- i + 1
      }
    }
  }
  
  l_x <- l_x[!is.na(l_x)]
  l_y <- l_y[!is.na(l_y)]
  l_symbol <- l_symbol[!is.na(l_symbol)]
  
  tibble(x=l_x, y=l_y, symbol=l_symbol)
}

reapply <- function(fun, init, n) {
  if (n == 1) return(fun(init))
  cli::cli_progress_bar("Reapplying...", total=n)
  out <- fun(init)
  for (i in 1:(n-1)) {
    cli::cli_progress_update()
    out <- fun(out)
  }
  cli::cli_progress_done()
  out
}


matches_to_df <- function(matches, fun=identity) {
  reduce(d14_match, rbind) |> data.frame() |> as.tibble() |> 
    select(-V1) |> mutate_all(fun)
}
