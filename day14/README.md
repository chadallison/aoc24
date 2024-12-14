Advent of Code: 14 December 2024
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
# part 1
input_clean = str_extract_all(input, "(-?)\\d+") |>
  (function(extracted_data) {
    tibble(pos_x = as.numeric(sapply(extracted_data, function(x) x[1])),
           pos_y = as.numeric(sapply(extracted_data, function(x) x[2])),
           vel_x = as.numeric(sapply(extracted_data, function(x) x[3])),
           vel_y = as.numeric(sapply(extracted_data, function(x) x[4])))
  })()

room_x = 101
room_y = 103

p1 = input_clean |> 
  mutate(pos_end_x = (pos_x + 100 * vel_x) %% room_x,
         pos_end_y = (pos_y + 100 * vel_y) %% room_y) |>
  mutate(quadrant = case_when(pos_end_x < 50 & pos_end_y < 51 ~ 1,
                              pos_end_x < 50 & pos_end_y > 51 ~ 2,
                              pos_end_x > 50 & pos_end_y < 51 ~ 3,
                              pos_end_x > 50 & pos_end_y > 51 ~ 4, T ~ 0)) |>
  filter(quadrant != 0) |>
  count(quadrant) |>
  pull(n) |>
  prod()

sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 214109808"

``` r
# part 2
find_unique_positions = function(input_clean, room_x, room_y) {
  t = 0
  curr_pos = input_clean
  while (t <= room_x * room_y) {
    t = t + 1
    curr_pos = curr_pos |>
      mutate(pos_x = (pos_x + vel_x) %% room_x,
             pos_y = (pos_y + vel_y) %% room_y)
    
    if (n_distinct(curr_pos[c("pos_x", "pos_y")]) == nrow(curr_pos)) {
      picture = matrix(" ", nrow = room_x, ncol = room_y)
      for (i in seq_len(nrow(curr_pos))) {
        picture[curr_pos$pos_x[i] + 1, curr_pos$pos_y[i] + 1] = "#"
      }
      
      out = apply(picture, function(x) { paste0(x, collapse = "") }, MARGIN = 1)
      return(list(time = t, output = out))
    }
  }
  return(NULL)
}

result = find_unique_positions(input_clean, room_x, room_y)
p2 = result$time
sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: 7687"
