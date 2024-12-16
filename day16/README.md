Advent of Code: 16 December 2024
================

``` r
library(tidyverse)
library(igraph)
```

``` r
input = data.frame(x = readLines("input.txt"))

# util functions
parse_txt = function(s, ...) {
  readr::parse_guess(s, locale = readr::locale("en", grouping_mark = ""))
}

grid_tidy = function(d, var, sep = "", parse = T) {
  ret = d |>
    mutate(row = row_number()) |>
    mutate(value = stringr::str_split({{ var }}, sep)) |>
    select(-{{ var }}) |>
    tidyr::unnest(value) |>
    group_by(row) |>
    mutate(col = row_number()) |>
    ungroup()

  if (parse) ret = ret |> mutate(value = parse_txt(value))
  ret
}

adjacent_join = function(x, y = x, diagonal = F, suffix = c("", "2")) {
  adj = tibble(row_delta = c(-1, 1, 0, 0), col_delta = c(0, 0, -1, 1))
  if (diagonal) adj = bind_rows(adj, tibble(row_delta = c(-1, -1, 1, 1), col_delta = c(-1, 1, -1, 1)))

  x |>
    tidyr::crossing(adj) |>
    mutate(row2 = row + row_delta,
           col2 = col + col_delta) |>
    inner_join(y, by = c(row2 = "row", col2 = "col"), suffix = suffix, relationship = "many-to-many") |>
    filter(row != row2 | col != col2) |>
    select(-row_delta, -col_delta)
}
```

``` r
# part 1
maze = input |>
  grid_tidy(x) |>
  filter(value != "#") |>
  tidyr::crossing(dir = c(">", "<", "^", "v")) |>
  mutate(name = paste(row, col, dir))

adjacent_moves = maze |>
  adjacent_join() |>
  filter((dir == dir2) & ((dir == "^" & row2 == row - 1) |
                            (dir == "v" & row2 == row + 1) |
                            (dir == "<" & col2 == col - 1) |
                            (dir == ">" & col2 == col + 1))) |>
  mutate(weight = 1)

turns = maze |>
  inner_join(maze, by = c("row", "col"), suffix = c("", "2"), relationship = "many-to-many") |>
  filter(dir != dir2) |>
  mutate(row2 = row, col2 = col) |>
  mutate(weight = 1000)

ig = bind_rows(adjacent_moves, turns) |>
  select(name, name2, weight) |>
  graph_from_data_frame()

start = maze |>
  filter(value == "S", dir == ">") |>
  pull(name)

ends = maze |>
  filter(value == "E") |>
  pull(name)

p1 = shortest_paths(ig, start, ends, output = "epath")$epath |>
  map_dbl(~ sum(edge_attr(ig, "weight")[.])) |>
  min()

sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 91464"

``` r
# part 2
p2 = all_shortest_paths(ig, start, ends)$res |>
  unlist() |>
  names() |>
  str_remove("[v\\^\\>\\<]") |>
  n_distinct()

sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: 494"
