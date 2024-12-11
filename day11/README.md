Advent of Code: 11 December 2024
================

``` r
library(tidyverse)
library(memoise)
```

``` r
input_raw = readLines("input.txt")
input = data.frame(x = input_raw)
```

``` r
# part 1
x = as.numeric(str_split(input$x, " ")[[1]])

blink_stones = memoise(function(stone, reps) {
  digits = floor(log10(stone)) + 1
  if (reps == 0) 1
  else if (stone == 0) blink_stones(1, reps - 1)
  else if (digits %% 2 == 0) {
    blink_stones(stone %/% 10 ^ (digits / 2), reps - 1) + blink_stones(stone %% 10 ^ (digits / 2), reps - 1)
  } else {
    blink_stones((stone * 2024), reps - 1)
  }
})

p1 = x |>
  map_dbl(blink_stones, 25) |>
  sum() |>
  as.character()

sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 218079"

``` r
# part 2
p2 = x |>
  map_dbl(blink_stones, 75) |>
  sum() |>
  as.character()

sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: 259755538429618"
