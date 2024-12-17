Advent of Code: 17 December 2024
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
to_binary = function(x) {
  result = numeric()
  number = x
  while (number != 0) {
    result = append(result, number %% 2)
    number = number %/% 2
  }
  return(rev(result))
}

binary_xor = function(x, y) {
  if (length(x) != length(y)) {
    n = abs(length(x) - length(y))
    if (length(x) < length(y)) {
      x = strsplit(strrep(0, n), "")[[1]] |> (\(.) c(., x))() |> as.numeric()
    } else {
      y = strsplit(strrep(0, n), "")[[1]] |> (\(.) c(., y))() |> as.numeric()
    }
  }
  
  output = numeric()
  for (i in seq_along(x)) output[i] = ifelse((x[i] == 0 & y[i] == 1) | (x[i] == 1 & y[i] == 0), 1, 0)
  output = rev(output)
  result = numeric()
  for (i in seq_along(output)) result[i] = output[i] * 2 ^ (i - 1)
  sum(result)
}

registers = input[seq_len(which(input == "") - 1)] |>
  (\(.) gsub(".*: ", "", .))() |>
  as.numeric() |>
  as.list() |>
  (\(.) { names(.) = c("A", "B", "C"); . })()

instructions = input[which(input == "") + 1] |>
  (\(.) gsub(".*: ", "", .))() |>
  (\(.) strsplit(., ",")[[1]])()

output = numeric()
i = 1

while (i < length(instructions)) {
  opcode = instructions[i]
  operand = as.numeric(instructions[i + 1])
  
  if (opcode == "0") {
    registers[["A"]] = floor(registers[["A"]] / (2 ^ c(0, 1, 2, 3, unlist(registers))[operand + 1]))
  } else if (opcode == "1") {
    registers[["B"]] = binary_xor(to_binary(registers[["B"]]), to_binary(operand))
  } else if (opcode == "2") {
    registers[["B"]] = c(0, 1, 2, 3, unlist(registers))[operand + 1] %% 8
  } else if (opcode == "3") {
    i = ifelse(registers[["A"]] == 0, i + 2, operand + 1)
    next
  } else if (opcode == "4") {
    registers[["B"]] = binary_xor(to_binary(registers[["B"]]), to_binary(registers[["C"]]))
  } else if (opcode == "5") {
    output = append(output, c(0, 1, 2, 3, unlist(registers))[operand + 1] %% 8)
  } else if (opcode == "6") {
    registers[["B"]] = floor(registers[["A"]] / (2 ^ c(0, 1, 2, 3, unlist(registers))[operand + 1]))
  } else if (opcode == "7") {
    registers[["C"]] = floor(registers[["A"]] / (2 ^ c(0, 1, 2, 3, unlist(registers))[operand + 1]))
  }
  i = i + 2
}

p1 = paste(output, collapse = ",")
sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 1,3,7,4,6,4,2,3,5"

``` r
get_candidates = function(vA) {
  registers = input[seq_len(which(input == "") - 1)] |>
    (\(.) gsub(".*: ", "", .))() |>
    as.numeric() |>
    as.list() |>
    (\(.) { names(.) = c("A", "B", "C"); . })()
  registers[["A"]] = vA
  
  output = numeric()
  i = 1
  
  while (i < length(instructions)) {
    opcode = instructions[i]
    operand = as.numeric(instructions[i + 1])
    
    if (opcode == "0") {
      registers[["A"]] = floor(registers[["A"]] / (2 ^ c(0, 1, 2, 3, unlist(registers))[operand + 1]))
    } else if (opcode == "1") {
      registers[["B"]] = binary_xor(to_binary(registers[["B"]]), to_binary(operand))
    } else if (opcode == "2") {
      registers[["B"]] = c(0, 1, 2, 3, unlist(registers))[operand + 1] %% 8
    } else if (opcode == "3") {
      i = ifelse(registers[["A"]] == 0, i + 2, operand + 1)
      next
    } else if (opcode == "4") {
      registers[["B"]] = binary_xor(to_binary(registers[["B"]]), to_binary(registers[["C"]]))
    } else if (opcode == "5") {
      output = append(output, c(0, 1, 2, 3, unlist(registers))[operand + 1] %% 8)
    } else if (opcode == "6") {
      registers[["B"]] = floor(registers[["A"]] / (2 ^ c(0, 1, 2, 3, unlist(registers))[operand + 1]))
    } else if (opcode == "7") {
      registers[["C"]] = floor(registers[["A"]] / (2 ^ c(0, 1, 2, 3, unlist(registers))[operand + 1]))
    }
    i = i + 2
  }
  
  output |> (\(.) { names(.) = NULL; . })()
}

calculate_level = function(...) {
  params = c(...)
  result = params[1]
  for (i in 2:length(params)) result = result * 8 + params[i]
  result
}

step = 1
candidates = expand.grid(0, 0:7)

while (step < length(instructions)) {
  candidates = apply(candidates, 1, function(x) {
    output1 = calculate_level(unlist(x)) |> get_candidates() |> paste(collapse = ";")
    output2 = instructions |> (\(.) .[(length(.) - step + 1):length(.)])() |> paste(collapse = ";")
    output1 == output2
  }, simplify = F) |> unlist() |> (\(.) candidates[., ])()
  
  candidates = merge(candidates |> (\(.) { names(.) = NULL; . })(), 0:7, all = T)
  step = step + 1
}

p2 = apply(candidates, 1, function(x) {
  output1 = calculate_level(unlist(x)) |> get_candidates() |> paste(collapse = ";")
  output2 = instructions |> (\(.) .[(length(.) - step + 1):length(.)])() |> paste(collapse = ";")
  output1 == output2
}, simplify = F) |> unlist() |> (\(.) candidates[., ])() |>
  apply(1, function(x) {
    calculate_level(unlist(x))
  }, simplify = F) |> unlist() |> min()

sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: 202367025818154"
