Advent of Code: 24 December 2024
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
# part 1
val = grep("\\d$", input[cumsum(input == "") == 0], value = T)
eq0 = grep("->", input, value = T)
eq = gsub("(.*) AND (.*) (-> .*)", "min(\\1, \\2) \\3", eq0)
eq = gsub("(.*) XOR (.*) (-> .*)", "(\\1 + \\2) %% 2L \\3", eq)
eq = gsub("(.*) OR (.*) (-> .*)",  "max(\\1, \\2) \\3", eq)

for (v in val) {
  evl = eval(parse(text = sub(":", "=", v)))
  eq = gsub(substr(v, 1, 3), evl, eq)
}


check = grep("\\(\\d.*\\d\\)", eq)

while (length(check) > 0) {
  for (k in check) {
    var = gsub(".* -> ", "", eq[k])
    evl = eval(parse(text = sub(":", "=", eq[k])))
    eq = gsub(var, evl, eq)
  }
  eq = eq[-check]
  check = grep("\\(\\d.*\\d\\)", eq)
}


res = sapply(ls(pattern = "^z"), get)
p1 = sum(res * 2^(seq_along(res) - 1))
sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 51410244478064"

``` r
# part 2
get_val = \(x) gsub(".* -> ", "", x)
mypad = \(n) ifelse(n <= 9L, paste0("0", n), as.character(n))
extract_gate = \(x) regmatches(x, gregexpr("[a-z]{3}", x))[[1]]
ends_with = \(x) grep(paste0(x, "$"), eq0, value = T)
res_z = get_val(eq0[!grepl("XOR", eq0) & grepl("z", eq0) & !grepl("z45", eq0)])
res_z_cor = character()

for (n in sub("z", "", res_z)) {
  tmp = eq0[grepl(paste0("x", n), eq0) & grepl(paste0("y", n), eq0) & grepl("XOR", eq0)]
  tmp2 = eq0[grepl(paste0(get_val(tmp), "."), eq0) & grepl("XOR", eq0)]
  res_z_cor = c(res_z_cor, get_val(tmp2))
}

for (k in seq_along(res_z)) {
  eq0 = gsub(paste0(res_z[k], "$"), paste0(res_z_cor[k], "999"), eq0)
  eq0 = gsub(paste0(res_z_cor[k], "$"), res_z[k], eq0)
  eq0 = gsub("999", "", eq0)
}

res = vector("list", 43)

for (k in 2:44)  {
  eq_z = grep(paste0("z", mypad(k)), eq0, value = T)
  eq_z2 = sapply(extract_gate(eq_z), ends_with)
  eq_z2 = eq_z2[order(!grepl("x\\d{2}", eq_z2))]
  eq_z3 = sapply(extract_gate(eq_z2[2])[1:2], ends_with)
  eq_z3 = eq_z3[order(!grepl("x\\d{2}", eq_z3))]
  res[[k - 1]] = c(eq_z, eq_z2, eq_z3)
}

res_xy = get_val(grep("XOR", sapply(res, \(z) z[2]), value = T, invert = T))
res_xy2 = get_val(grep("AND", sapply(res, \(z) z[4]), value = T, invert = T))
p2 = paste0(sort(c(res_z, res_z_cor, res_xy, res_xy2)), collapse = ",")
sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: gst,khg,nhn,tvb,vdc,z12,z21,z33"
