Advent of Code: 23 December 2024
================

``` r
library(tidyverse)
```

``` r
input = as.matrix(read.table("input.txt", sep = "-"))
```

``` r
# part 1
count_triple = function(comp) {
  nxt = as.character(input[input[, 1] %in% comp | input[, 2] %in% comp, ])
  tab = split(nxt, nxt)
  n_t = pmin(3L, length(grep("^t", comp)) + grepl("^t", names(tab)) + 1L)
  sum(ifelse(lengths(tab) == 2L, 1L, 0L) / n_t)
}

t_rows = input[grepl("^t", input[,1]) | grepl("^t", input[,2]), ]
p1 = sum(apply(t_rows, 1, count_triple))
sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 1323"

``` r
# part 2
netw = rbind(input, input[, 2:1])

find_cluster = function(comp) {
  nxt = netw[netw[, 1] == comp, 2L]
  nxtnxt = sapply(nxt, \(x) netw[netw[, 1] %in% x, 2L])
  res = sapply(nxt, \(x) sum(colSums(nxtnxt == x)))
  c(comp, names(res[res == max(res)]))
}

to_check = unique(netw[, 1])
cl_list = list()

while (length(to_check > 1)) {
  cl_list = c(list(find_cluster(to_check[1])), cl_list)
  to_check = setdiff(to_check, cl_list[[1]])
}

p2 = paste0(sort(cl_list[[which.max(lengths(cl_list))]]), collapse = ",")
sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: er,fh,fi,ir,kk,lo,lp,qi,ti,vb,xf,ys,yu"
