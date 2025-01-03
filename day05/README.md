Advent of Code: 5 December 2024
================

``` r
library(tidyverse)
```

``` r
input = "input.txt"
skip = which(readLines(input) == "")
```

``` r
# part 1
dt1 = readr::read_delim(input, "|", col_names = F, n_max = skip - 1, show_col_types = F) |>
  dplyr::filter(!is.na(X2))

dt2 = readLines(input)[(skip + 1):length(readLines(input))] |>
  strsplit(",") |>
  purrr::map(as.integer)

p1 = sum(dt2 |> purrr::map_int(\(x) {
  if (length(x) %% 2 == 0) print(x)
  is_valid = seq_along(x) |>
    purrr::map_lgl(\(i) {
      if (i == length(x)) T else {
        left = x[i]; right = x[(i + 1):length(x)]
        all(right %in% dt1$X2[dt1$X1 == left])
      }
    }) |> all()
  if (is_valid) x[ceiling(length(x) / 2)] else 0
}))

sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 4905"

``` r
# part 2
swap = function(x, i = 1) {
  left = x[i]; right = x[i + 1]
  check_right = dt1$X2[dt1$X1 == left]
  if (is.na(right)) return(x)
  if (!(right %in% check_right)) {
    x[i + 1] = left; x[i] = right
    swap(x, 1)
  } else { swap(x, i + 1) }
}

middle_page = dt2 |> purrr::map_int(\(x) {
  if (length(x) %% 2 == 0) return(0)
  is_valid = seq_along(x) |>
    purrr::map_lgl(\(i) {
      if (i == length(x)) T else {
        left = x[i]; right = x[(i + 1):length(x)]
        all(right %in% dt1$X2[dt1$X1 == left])
      }
    }) |> all()
  if (is_valid) x[ceiling(length(x) / 2)] else 0
})

p2 = dt2[middle_page == 0] |>
  purrr::map_int(\(x) {
    x = swap(x)
    x[ceiling(length(x) / 2)]
  }) |>
  sum()

sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: 6204"
