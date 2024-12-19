Advent of Code: 19 December 2024
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
# part 1
pattern = strsplit(input[1], ", ")[[1]] |>
  paste(collapse = "|") |> 
  (\(.) { paste0("^(", ., ")*$") })()

p1 = sum(grepl(pattern, input[3:length(input)]))
sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 360"

``` r
# part 2
count_combinations = function(word, elements) {
  n = nchar(word)
  memory = vector("numeric", n + 1)
  memory[1] = 1

  for (j in seq_len(n)) {
    for (k in elements) {
      length_k = nchar(k)
      if (j >= length_k) {
        substring = substr(word, j - length_k + 1, j)
        if (substring == k) {
          memory[j + 1] = memory[j + 1] + memory[j - length_k + 1]
        }
      }
    }
  }
  memory[n + 1]
}

results = vector("list")

for (i in input[3:length(input)][grepl(pattern, input[3:length(input)])]) {
  pattern_parts = strsplit(input[1], ", ")[[1]] |> 
    lapply(function(x) { grepl(x, i) }) |> 
    unlist() |> 
    (\(.) { strsplit(input[1], ", ")[[1]][.] })()

  results = append(results, count_combinations(i, pattern_parts))
}

p2 = sum(unlist(results))
sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: 577474410989846"
