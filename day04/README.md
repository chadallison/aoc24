Advent of Code: 4 December 2024
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
# part 1
dt_split = strsplit(input, "")
dt = matrix(unlist(dt_split), ncol = length(dt_split[[1]]), byrow = TRUE)

get_diagonal = function(m, x, dim = c("row", "col"), dir = c("up", "down")) {
  dim = rlang::arg_match(dim)
  dir = rlang::arg_match(dir)
  if (dir == "up") m = m[nrow(m):1, , drop = FALSE]
  sub_matrix = if (dim == "row") m[x:nrow(m), 1:(ncol(m) - x + 1), drop = FALSE] 
               else m[1:(nrow(m) - x + 1), x:ncol(m), drop = FALSE]
  if (length(sub_matrix) == 1) sub_matrix else diag(sub_matrix)
}

strg_extract_all = function(str, pattern) {
  patterns = stringi::stri_split_regex(pattern, "\\|", simplify = TRUE) |> as.vector()
  purrr::map(str, \(x) {
    matches = stringi::stri_locate_all_regex(x, patterns, omit_no_match = TRUE) |>
      purrr::reduce(Matrix::rbind2)
    stringi::stri_sub(x, matches[, 1], matches[, 2])
  })
}

count = function(x) {
  paste0(x, collapse = "") |>
    strg_extract_all("XMAS|SAMX") |>
    unlist() |>
    length()
}

count_diag = function(indices, dim, dir) {
  purrr::map_int(indices, \(x) get_diagonal(dt, x, dim, dir) |> count()) |> sum()
}

total_count = sum(
  apply(dt, 1, count), 
  apply(dt, 2, count), 
  count_diag(seq_len(nrow(dt)), "row", "down"), 
  count_diag(seq_len(ncol(dt))[-1], "col", "down"), 
  count_diag(seq_len(nrow(dt)), "row", "up"), 
  count_diag(seq_len(ncol(dt))[-1], "col", "up")
)

sprintf("Part 1 solution: %s", total_count)
```

    ## [1] "Part 1 solution: 2718"

``` r
# part 2
dt_split = strsplit(input, "")
dt = matrix(unlist(dt_split), ncol = length(dt_split[[1]]), byrow = TRUE)

check_x_mas = function(i, j) {
  if (i < 2 | j < 2 | i > nrow(dt) - 1 | j > ncol(dt) - 1) return(0)
  diag1 = sapply(-1:1, \(d) dt[i + d, j + d])
  diag2 = sapply(-1:1, \(d) dt[i + d, j - d])
  diag1_matches = paste(diag1, collapse = "") %in% c("MAS", "SAM")
  diag2_matches = paste(diag2, collapse = "") %in% c("MAS", "SAM")
  return(as.integer(diag1_matches && diag2_matches))
}

total_x_mas = sum(sapply(2:(nrow(dt) - 1), function(i) {
  sum(sapply(2:(ncol(dt) - 1), function(j) check_x_mas(i, j)))
}))

sprintf("Part 2 solution: %s", total_x_mas)
```

    ## [1] "Part 2 solution: 2046"
