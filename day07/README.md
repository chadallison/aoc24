Advent of Code: 7 December 2024
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
input_df = data.frame(x = input)
```

``` r
parse_input = function(filename) {
  map(readLines(filename) |> str_trim() |> discard(~ . == ""), ~ {
    parts = strsplit(.x, ": ")[[1]]
    list(result = as.numeric(parts[1]), numbers = as.numeric(strsplit(parts[2], " ")[[1]]))
  })
}

compute_result = function(numbers, idx = 1, current_result = numbers[1]) {
  if (idx == length(numbers)) return(current_result)
  result_add = compute_result(numbers, idx + 1, current_result + numbers[idx + 1])
  result_mul = compute_result(numbers, idx + 1, current_result * numbers[idx + 1])
  return(c(result_add, result_mul))
}

solve = function() {
  input = parse_input("input.txt")
  total = 0
  
  for (item in input) {
    result = item$result
    numbers = item$numbers
    possible_results = compute_result(numbers)
    if (result %in% possible_results) total = total + result
  }
  return(total)
}

p1 = solve()
sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 882304362421"

``` r
# part 2
is_possible = function(value, numbers) {
  if (length(numbers) == 1) return(value == numbers)
  last = numbers[length(numbers)]
  rest = numbers[-length(numbers)]
  
  return(
    (last <= value && is_possible(value - last, rest)) ||
    (value %% last == 0 && is_possible(value / last, rest)) ||
    (str_detect(value, paste0(".+", last, "$")) &&
       is_possible(as.numeric(str_sub(value, 1, nchar(value) - nchar(last))), rest))
  )
}

p2 = input_df |>
  separate(x, c("value", "numbers"), sep = ": ", convert = T) |>
  mutate(numbers = str_split(numbers, " ") |> map(as.numeric)) |>
  filter(map2_lgl(value, numbers, is_possible)) |>
  summarize(x = as.character(sum(value))) |>
  pull(x)

sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: 145149066755184"
