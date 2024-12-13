Advent of Code: 13 December 2024
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
# part 1
tol = 10 ^ -13

input_clean = input |> 
  str_extract_all("-?\\d+") |> 
  (\(data) tibble(x = sapply(data, \(x) x[1]), y = sapply(data, \(x) x[2])))() |> 
  mutate(arcade_id = cumsum(is.na(x)) + 1) |> 
  filter(!is.na(x)) |> 
  mutate(tmp = case_when(row_number() == 1 ~ "a", row_number() == 2 ~ "b", row_number() == 3 ~ "goal"), .by = arcade_id) |> 
  pivot_wider(id_cols = arcade_id, values_from = c(x, y), names_from = tmp) |> 
  mutate(across(where(is.character), as.double))

p1 = input_clean |> 
  (\(data) data |> mutate(click_a = (data$x_goal * data$y_b - data$y_goal * data$x_b) / (data$x_a * data$y_b - data$y_a * data$x_b),
                          click_b = (data$x_goal * data$y_a - data$y_goal * data$x_a) / (data$x_b * data$y_a - data$y_b * data$x_a)))() |> 
  filter(((click_a %% 1 < tol) | (-click_a %% 1 < tol)) & ((click_b %% 1 < tol) | (-click_b %% 1 < tol))) |> 
  mutate(cost = 3 * click_a + 1 * click_b) |> 
  summarise(ans = sum(cost)) |> 
  pull(ans)

sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 34787"

``` r
# part 2
p2 = input_clean |> 
  (\(data) data |> mutate(x_goal = data$x_goal + 10 ^ 13, y_goal = data$y_goal + 10 ^ 13))() |> 
  (\(data) data |> mutate(click_a = (data$x_goal * data$y_b - data$y_goal * data$x_b) / (data$x_a * data$y_b - data$y_a * data$x_b),
                          click_b = (data$x_goal * data$y_a - data$y_goal * data$x_a) / (data$x_b * data$y_a - data$y_b * data$x_a)))() |> 
  filter(((click_a %% 1 < tol) | (-click_a %% 1 < tol)) & ((click_b %% 1 < tol) | (-click_b %% 1 < tol))) |> 
  mutate(cost = 3 * click_a + 1 * click_b) |> 
  summarise(ans = sum(cost)) |> 
  pull(ans) |>
  as.character()


sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: 85644161121698"
