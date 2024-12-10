Advent of Code: 10 December 2024
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
input_clean = input |>
  str_split(pattern = "") |>
  map(~ as.numeric(.)) |>
  simplify2array()

heights_data = expand_grid(row = seq_len(nrow(input_clean)),
                           col = seq_len(ncol(input_clean))) |>
  mutate(pos = row + col * 1i,
         height = input_clean[cbind(row, col)]) |>
  select(pos, height)

current_paths = heights_data |>
  filter(height == 0) |>
  transmute(start_pos = pos, curr_pos = pos, curr_height = height)

take_a_step = function(current_paths, heights_data) {
  heights_data = heights_data |>
    rename(next_height = height, next_pos = pos)

  current_paths |>
    inner_join(heights_data, by = character()) |>
    filter(next_height == curr_height + 1, Mod(next_pos - curr_pos) == 1) |>
    transmute(start_pos, curr_pos = next_pos, curr_height = next_height)
}

for (i in seq_len(9)) {
  current_paths = take_a_step(current_paths, heights_data)
}
```

    ## Warning: Using `by = character()` to perform a cross join was deprecated in dplyr 1.1.0.
    ## ℹ Please use `cross_join()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
p1 = current_paths |>
  distinct(start_pos, curr_pos) |>
  nrow()

sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 786"

``` r
sprintf("Part 2 solution: %s", nrow(current_paths))
```

    ## [1] "Part 2 solution: 1722"
