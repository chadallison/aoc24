Advent of Code: 25 December 2024
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
patterns = input |> 
  paste(collapse = "\n") |> 
  strsplit("\n\n") |> 
  unlist() |> 
  lapply(function(pattern) {
    lines = strsplit(pattern, "\n")[[1]]
    positions = which(sapply(lines, function(line) unlist(strsplit(line, NULL))) == "#", arr.ind = T)
    as.data.frame(positions)
  })

count = 0
combinations = combn(seq_along(patterns), 2)

for (idx in 1:ncol(combinations)) {
  pattern1 = patterns[[combinations[1, idx]]]
  pattern2 = patterns[[combinations[2, idx]]]
  if (!any(duplicated(rbind(pattern1, pattern2)))) count = count + 1
}

sprintf("Part 1 solution: %s", count)
```

    ## [1] "Part 1 solution: 2885"
