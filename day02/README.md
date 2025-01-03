Advent of Code: 2 December 2024
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
# part 1
safe_cnt = 0

for (inp in input) {
  grp = as.numeric(unlist(strsplit(inp, " ")))
  diffs = diff(grp)
  
  if ((all(diffs %in% 1:3)) | (all(diffs %in% -3:-1))) {
    safe_cnt = safe_cnt + 1
  }
}

sprintf("Part 1 solution: %s", safe_cnt)
```

    ## [1] "Part 1 solution: 639"

``` r
# part 2
safe_cnt = 0

for (inp in input) {
  grp = as.numeric(unlist(strsplit(inp, " ")))
  diffs = diff(grp)
  
  if ((all(diffs %in% 1:3)) | (all(diffs %in% -3:-1))) {
    safe_cnt = safe_cnt + 1
  } else {
    for (i in seq_along(grp)) {
      sub_grp = grp[-i]
      sub_diffs = diff(sub_grp)
      
      if ((all(sub_diffs %in% 1:3)) | (all(sub_diffs %in% -3:-1))) {
        safe_cnt = safe_cnt + 1
        break
      }
    }
  }
}

sprintf("Part 2 solution: %s", safe_cnt)
```

    ## [1] "Part 2 solution: 674"
