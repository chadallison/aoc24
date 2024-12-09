Advent of Code: 8 December 2024
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
# parts 1 and 2
input = readLines("input.txt")

size = length(input)

input_clean = input |>
  str_split("") |>
  unlist() |>
  matrix(nrow = size, byrow = T)

antenna_locs = which(str_detect(input_clean, "[a-zA-Z0-9]")) |>
  tibble(raw = _) |>
  mutate(
    x = (raw - 1) %% size + 1,
    y = (raw - 1) %/% size + 1,
    antenna = mapply(\(x, y) input_clean[x, y], x, y, SIMPLIFY = T)
  ) |>
  select(-raw)

resonant_pairs = inner_join(
  antenna_locs |> rename(x1 = x, y1 = y),
  antenna_locs |> rename(x2 = x, y2 = y),
  by = join_by(antenna),
  relationship = "many-to-many"
) |>
  filter(!(x1 == x2 & y1 == y2))

resonant_locs = resonant_pairs |>
  crossing(resonants = -size:size) |>
  mutate(
    anti_x = x1 + resonants * (x1 - x2),
    anti_y = y1 + resonants * (y1 - y2)
  ) |>
  filter(anti_x %in% 1:size & anti_y %in% 1:size)

p1 = resonant_locs |>
  filter(resonants %in% c(-2, 1)) |>
  distinct(anti_x, anti_y) |>
  nrow()

p2 = resonant_locs |>
  distinct(anti_x, anti_y) |>
  nrow()

sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 273"

``` r
sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: 1017"
