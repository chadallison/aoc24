Advent of Code: 6 December 2024
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
# part 1
input_clean = input |>
  str_split("") |>
  unlist() |>
  (\(x) matrix(x, nrow = length(input), byrow = T))() |>
  (\(x) cbind(matrix(rep("O", nrow(x)), ncol = 1), x, matrix(rep("O", nrow(x)), ncol = 1)))() |>
  (\(x) rbind(matrix(rep("O", ncol(x)), nrow = 1), x, matrix(rep("O", ncol(x)), nrow = 1)))()

directions = list("N" = c(-1, 0), "E" = c(0, 1), "S" = c(1, 0), "W" = c(0, -1))
turn_right = list("N" = "E", "E" = "S", "S" = "W", "W" = "N")

tiles_to_get_out = function(curr_map, current_pos, current_dir) {
  repeat {
    if (str_detect(curr_map[current_pos[1], current_pos[2]], current_dir)) {
      return(-1)
    } else {
      curr_map[current_pos[1], current_pos[2]] = paste0(curr_map[current_pos[1], current_pos[2]], current_dir)
    }
    tile_next = curr_map[current_pos[1] + directions[[current_dir]][1], current_pos[2] + directions[[current_dir]][2]]
    if (tile_next == "O") {
      return(curr_map[str_detect(curr_map, "[NESW]")] %>% length())
    } else if (tile_next == "#") {
      current_dir = turn_right[[current_dir]]
    } else {
      current_pos = current_pos + directions[[current_dir]]
    }
  }
}

p1 = tiles_to_get_out(curr_map = input_clean, current_pos = which(input_clean == "^", arr.ind = T) %>% as.vector, current_dir = "N")
sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 5453"

``` r
curr_map = input_clean
current_pos = which(input_clean == "^", arr.ind = T) |> as.vector()
current_dir = "N"
p2 = 0
visited_positions = character()

repeat {
  position_key = paste0(current_pos[1], ",", current_pos[2], ",", current_dir)
  if (position_key %in% visited_positions) break
  visited_positions = c(visited_positions, position_key)
  curr_map[current_pos[1], current_pos[2]] = paste0(curr_map[current_pos[1], current_pos[2]], current_dir)
  tile_next = curr_map[current_pos[1] + directions[[current_dir]][1], current_pos[2] + directions[[current_dir]][2]]
  if (tile_next == "O") {
    break
  } else if (tile_next == "#") {
    current_dir = turn_right[[current_dir]]
  } else {
    if (tile_next == ".") {
      tmp_pos = current_pos + directions[[current_dir]]
      tmp_map = curr_map
      tmp_map[tmp_pos[1], tmp_pos[2]] = "#"
      if (tiles_to_get_out(tmp_map, current_pos, turn_right[[current_dir]]) == -1) {
        p2 = p2 + 1
      }
    }
    current_pos = current_pos + directions[[current_dir]]
  }
}

sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: 2188"
