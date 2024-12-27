Advent of Code: 25 December 2024
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
# part 1
patterns = input |>
  paste(collapse = "\n") |>
  strsplit("\n\n") |>
  unlist() |>
  lapply(function(pattern) {
    lines = strsplit(pattern, "\n")[[1]]
    positions = which(sapply(lines, function(line) unlist(strsplit(line, NULL))) == "#", arr.ind = T)
    positions = as.matrix(positions)  # Use matrix for faster operations
    positions
  })

combinations = combn(seq_along(patterns), 2)

unique_patterns = lapply(patterns, function(pattern) {
  apply(pattern, 1, function(row) paste(row, collapse = ","))
})

count = 0

for (idx in 1:ncol(combinations)) {
  pattern1 = unique_patterns[[combinations[1, idx]]]
  pattern2 = unique_patterns[[combinations[2, idx]]]
  combined = c(pattern1, pattern2)
  if (length(combined) == length(unique(combined))) {
    count = count + 1
  }
}

sprintf("Part 1 solution: %s", count)
```

    ## [1] "Part 1 solution: 2885"
