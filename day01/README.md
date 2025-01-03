Advent of Code: 1 December 2024
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
# part 1
df = data.frame(x = input) |>
  separate(x, into = c("l", "r"), sep = "  ", convert = T)

p1 = data.frame(new_l = sort(df$l), new_r = sort(df$r)) |>
  mutate(diff = abs(new_l - new_r)) |>
  summarise(x = sum(diff)) |>
  pull(x)

sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 1889772"

``` r
# part 2
r_cnt = df |>
  count(r, name = "cnt")

p2 = df |>
  left_join(r_cnt, by = c("l" = "r")) |>
  mutate(cnt = coalesce(cnt, 0),
         value = l * cnt) |>
  summarise(x = sum(value)) |>
  pull(x)

sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: 23228917"
