Advent of Code: 15 December 2024
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
# part 1
n = 50L

dir_vec = unname(c("^" = -1i, "v" = 1i, ">" = 1, "<" = -1)[
    unlist(strsplit(input[-(1:(n + 1))], ""))
])

# Parse input grid and extract positions
x = as.character(do.call(rbind, strsplit(input[1:n], "")))
co = rep(seq_len(n) - 1, each = n) + rep(seq_len(n) - 1, n) * 1i

wall = co[x == "#"]
box = co[x == "O"]
pos = co[x == "@"]

# Simulation loop
for (dir in dir_vec) {
    new_pos = pos + dir
    if (!new_pos %in% wall) {
        box2 = if (Im(dir) == 0) box[Im(box) == Im(pos)] else box[Re(box) == Re(pos)]
        if (new_pos %in% box2) {
            n_box = Position(\(k) !(pos + k * dir) %in% box2, 1:n) - 1L
            if (!(pos + (n_box + 1L) * dir) %in% wall) {
                mbox = pos + seq_len(n_box) * dir  # Boxes to be moved
                box[box %in% mbox] = mbox + dir
                pos = new_pos
            }
        } else {
            pos = new_pos
        }
    }
}

p1 = sum(100 * Im(box) + Re(box))
sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 1349898"

``` r
# part 2
update_tile = function(x) {
    gsub("#", "##", gsub("O", "[]", gsub("@", "@.", gsub("\\.", "..", x))))
}

x = as.character(do.call(rbind, strsplit(update_tile(input[1:n]), "")))
co = rep(seq_len(2 * n) - 1, each = n) + rep(seq_len(n) - 1, 2 * n) * 1i
wall = co[x == "#"]
box_l = co[x == "["]
box_r = box_l + 1
box = c(box_l, box_r)
pos = co[x == "@"]

for (dir in dir_vec) {
    new_pos = pos + dir
    if (!new_pos %in% wall) {
        if (Im(dir) == 0) {
            box2 = box[Im(box) == Im(pos)]
            if (new_pos %in% box2) {
                n_box = Position(\(k) !(pos + k * dir) %in% box2, 1:n) - 1L
                if (!(pos + (n_box + 1L) * dir) %in% wall) {
                    mbox = pos + seq_len(n_box) * dir  # Boxes to be moved
                    box_l[box_l %in% mbox] = box_l[box_l %in% mbox] + dir
                    box_r[box_r %in% mbox] = box_r[box_r %in% mbox] + dir
                    box = c(box_l, box_r)
                    pos = new_pos
                }
            } else {
                pos = new_pos
            }
        } else {
            box2 = box[Re(box) == Re(pos)]
            if (new_pos %in% box2) {
                mbox = new_pos + c(0, if (new_pos %in% box_l) 1 else -1)
                move = TRUE
                n_box = mbox

                while (TRUE) {
                    if (any((n_box + dir) %in% wall)) {
                        move = FALSE
                        break
                    }
                    n_box = box[box %in% (n_box + dir)]
                    if (length(n_box) == 0) break

                    n_box = n_box[order(Re(n_box))]
                    if (n_box[1] %in% box_r) n_box = c(n_box[1] - 1, n_box)
                    if (n_box[length(n_box)] %in% box_l) n_box = c(n_box, n_box[length(n_box)] + 1)
                    mbox = c(mbox, n_box)
                }

                if (move) {
                    box_l[box_l %in% mbox] = box_l[box_l %in% mbox] + dir
                    box_r[box_r %in% mbox] = box_r[box_r %in% mbox] + dir
                    box = c(box_l, box_r)
                    pos = new_pos
                }
            } else {
                pos = new_pos
            }
        }
    }
}

p2 = sum(100 * Im(box_l) + Re(box_r) - 1)
sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: 1376686"
