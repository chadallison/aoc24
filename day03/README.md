Advent of Code: 3 December 2024
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
# part 1
muls = unlist(str_extract_all(input, "mul\\(\\d+,\\d+\\)"))

p1 = data.frame(x = muls) |>
  mutate(x = str_remove_all(x, "mul|\\(|\\)")) |>
  separate(x, into = c("a", "b"), sep = ",", convert = T) |>
  mutate(prod = a * b) |>
  summarise(total = sum(prod)) |>
  pull(total)

sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 174960292"

``` r
# part 2
instructions = unlist(str_extract_all(input, "mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)"))
df = data.frame(x = instructions)

df$keep = T
current_keep_state = T

for (i in seq_len(nrow(df))) {
  if (df$x[i] == "do()") {
    current_keep_state = T
  } else if (df$x[i] == "don't()") {
    current_keep_state = F
  }
  df$keep[i] = current_keep_state
}

p2 = df |>
  filter(keep == T & x != "do()") |>
  mutate(x = str_remove_all(x, "mul\\(|\\)")) |>
  separate(x, into = c("a", "b"), sep = ",", convert = T) |>
  mutate(prod = a * b) |>
  summarise(total = sum(prod)) |>
  pull(total)

sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: 56275602"
