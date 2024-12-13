Advent of Code: 12 December 2024
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
input_clean = input |>
  str_split("") |>
  unlist() |>
  (\(x) matrix(x, nrow = length(input), byrow = T))() |>
  (\(x) cbind(matrix(rep(".", nrow(x)), ncol = 1), x, matrix(rep(".", nrow(x)), ncol = 1)))() |>
  (\(x) rbind(matrix(rep(".", ncol(x)), nrow = 1), x, matrix(rep(".", ncol(x)), nrow = 1)))()

garden_data = crossing(tibble(pos_x = 1:nrow(input_clean)),
                       tibble(pos_y = 1:ncol(input_clean))) |>
  mutate(garden = mapply(\(x, y) { input_clean[x, y] }, pos_x, pos_y, SIMPLIFY = T)) |>
  select(pos_x, pos_y, garden) |>
  mutate(id = ifelse(garden == ".", 0, row_number())) |>
  arrange(pos_x, pos_y)

repeat {
  next_garden = garden_data |>
    inner_join(garden_data |> 
                 rename(pos_x_nbr = pos_x, pos_y_nbr = pos_y, id_neighbour = id) |>
                 mutate(pos_x_low = pos_x_nbr - 1,
                        pos_x_hi  = pos_x_nbr + 1,
                        pos_y_low = pos_y_nbr - 1,
                        pos_y_hi  = pos_y_nbr + 1),
               by = join_by(garden, between(pos_x, pos_x_low, pos_x_hi),between(pos_y, pos_y_low, pos_y_hi)), relationship = "many-to-many") |>
    filter(abs(pos_x - pos_x_nbr) + abs(pos_y - pos_y_nbr) <= 1) |>
    summarise(id = min(id_neighbour), .by = c(pos_x, pos_y, garden)) |>
    arrange(pos_x, pos_y)
  
  if (isTRUE(all.equal(garden_data, next_garden))) {
    break
  } else {
    garden_data = next_garden
  }
}

final_gardens = next_garden
areas = final_gardens |> count(id, name = "area") |> filter(id > 0)

fences = final_gardens |>
  filter(garden != ".") |>
  inner_join(final_gardens |>
               rename(pos_x_nbr = pos_x, pos_y_nbr = pos_y, id_neighbour = id, garden_nbr = garden) |>
               mutate(pos_x_low = pos_x_nbr - 1,
                      pos_x_hi  = pos_x_nbr + 1,
                      pos_y_low = pos_y_nbr - 1,
                      pos_y_hi  = pos_y_nbr + 1), 
             by = join_by(between(pos_x, pos_x_low, pos_x_hi), between(pos_y, pos_y_low, pos_y_hi)), relationship = "many-to-many") |>
  filter((abs(pos_x - pos_x_nbr) + abs(pos_y - pos_y_nbr) == 1) & (garden != garden_nbr))

perimeter = fences |>
  summarise(perimeter = n(), .by = id)

p1 = areas |>
  left_join(perimeter, by = join_by(id)) |>
  summarise(ans = sum(area * perimeter)) |>
  pull(ans)

sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 1465968"

``` r
# part 2
sides = fences |>
  select(pos_x, pos_y, pos_x_nbr, pos_y_nbr, id, id_neighbour) |>
  mutate(type = case_when(pos_x == pos_x_nbr ~ "-", pos_y == pos_y_nbr ~ "|", T ~ "Panic!"),
         pos_x_final = pmin(pos_x, pos_x_nbr),
         pos_y_final = pmin(pos_y, pos_y_nbr)) |>
  select(id, type, pos_x = pos_x_final, pos_y = pos_y_final) |>
  mutate(id_side = row_number())

repeat {
  next_sides = sides |>
    inner_join(sides |> 
                 rename(pos_x_nbr = pos_x, pos_y_nbr = pos_y, id_side_nbr = id_side) |>
                 mutate(pos_x_low = pos_x_nbr - 1,
                        pos_x_hi  = pos_x_nbr + 1,
                        pos_y_low = pos_y_nbr - 1,
                        pos_y_hi  = pos_y_nbr + 1), 
               by = join_by(id, type, between(pos_x, pos_x_low, pos_x_hi), between(pos_y, pos_y_low, pos_y_hi)), relationship = "many-to-many") |>
    filter((type == "|" & (pos_x == pos_x_nbr) & abs(pos_y - pos_y_nbr) <= 1) | (type == "-" & (pos_y == pos_y_nbr) & abs(pos_x - pos_x_nbr) <= 1)) |>
    summarise(id_side = min(id_side_nbr), .by = c(pos_x, pos_y, id, type)) |>
    arrange(pos_x, pos_y)
  
  if (isTRUE(all.equal(sides, next_sides))) {
    break
  } else {
    sides = next_sides
  }
}

final_sides = next_sides
perimeter_bulk = final_sides |> summarise(perimeter_bulk = n_distinct(id_side), .by = id)

cross_detangling_bugfix = final_sides |>
  count(pos_x, pos_y, id) |>
  filter(n == 2) |>
  select(-n) |>
  mutate(pos_downright_x = pos_x + 1,
         pos_downright_y = pos_y + 1) |>
  semi_join(final_sides |> filter(type == "|"), by = join_by(id, pos_x, pos_downright_y == pos_y)) |>
  semi_join(final_sides |> filter(type == "-"), by = join_by(id, pos_y, pos_downright_x == pos_x)) |>
  summarise(extra_walls = 2 * n(), .by = id)

p2 = areas |>
    left_join(perimeter_bulk, by = join_by(id)) |>
    left_join(cross_detangling_bugfix, by = join_by(id)) |>
    mutate(extra_walls = coalesce(extra_walls,0)) |>
    summarise(ans = sum(area * (perimeter_bulk + extra_walls))) |>
    pull(ans)

sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: 897702"
