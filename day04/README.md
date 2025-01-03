Advent of Code: 4 December 2024
================

``` r
library(tidyverse)
```

``` r
input = read.delim("input.txt", header = F)
```

``` r
# part 1
mat = str_split_fixed(input$V1, "", nchar(input[1, ]))

d1 = row(mat) - col(mat)
diag1 = split(mat, d1)
diag2 = split(mat[nrow(mat):1, ], d1)

sol = c()
for (i in 1:nrow(mat)) {
  row = str_extract_all(paste0(mat[i, ], collapse = ""), "XMAS", simplify = TRUE)
  row1 = str_extract_all(paste0(mat[i, ], collapse = ""), "SAMX", simplify = TRUE)
  col = str_extract_all(paste0(mat[, i], collapse = ""), "XMAS", simplify = TRUE)
  col1 = str_extract_all(paste0(mat[, i], collapse = ""), "SAMX", simplify = TRUE)
  sol = c(sol, row, row1, col, col1)
}

for (i in 1:length(diag1)) {
  sol = c(sol, 
          str_extract_all(paste0(diag1[[i]], collapse = ""), "XMAS", simplify = TRUE),
          str_extract_all(paste0(diag1[[i]], collapse = ""), "SAMX", simplify = TRUE),
          str_extract_all(paste0(diag2[[i]], collapse = ""), "XMAS", simplify = TRUE),
          str_extract_all(paste0(diag2[[i]], collapse = ""), "SAMX", simplify = TRUE))
}

p1 = length(sol)
sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 2718"

``` r
# part 2
p2 = sum(sapply(1:(nrow(mat) - 2), function(i) {
  sapply(1:(ncol(mat) - 2), function(j) {
    p2 = mat[i:(i + 2), j:(j + 2)]
    p2.2 = p2[nrow(p2):1, ]
    if (p2[2, 2] == "A" && 
        (paste0(diag(p2), collapse = "") %in% c("MAS", "SAM")) && 
        (paste0(diag(p2.2), collapse = "") %in% c("MAS", "SAM"))) {
      return(1)
    }
    return(0)
  })
}))

sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: 2046"
