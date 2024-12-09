Advent of Code: 8 December 2024
================

``` r
library(tidyverse)
```

``` r
disk = readLines("input.txt")
```

``` r
# part 1
parse_disk_data = function(disk_data) {
    as.integer(unlist(strsplit(disk_data, "", fixed = T)))
}

generate_id_sequence = function(disk_space) {
    seq_id = rep(c(1L, -1L), times = length(disk_space) %/% 2L)
    if (length(disk_space) %% 2L == 1L) {
        seq_id = c(seq_id, 1L)
    }
    seq_id[seq_id == 1L] = seq_along(seq_id[seq_id == 1L]) - 1L
    seq_id
}

map_blocks = function(disk_space, id_seq) {
    blocks = integer(sum(disk_space))
    blocks[blocks == 0L] = -1L
    index = 1L
    for (i in seq_along(disk_space)) {
        blocks[index - 1L + seq_len(disk_space[i])] = id_seq[i]
        index = index + disk_space[i]
    }
    blocks
}

compact_blocks = function(blocks) {
    empty_count = sum(blocks == -1L)
    filled_blocks = blocks[blocks != -1L]
    blocks[blocks == -1L] = rev(filled_blocks)[seq_len(empty_count)]
    blocks[seq_along(filled_blocks)]
}

calculate_checksum = function(compacted_blocks) {
    compacted_blocks[compacted_blocks == -1L] = 0L
    sum(compacted_blocks * (seq_along(compacted_blocks) - 1L))
}

solve_disk_problem = function(disk_data) {
    disk_space = parse_disk_data(disk_data)
    id_seq = generate_id_sequence(disk_space)
    disk_space |>
        map_blocks(id_seq) |>
        compact_blocks() |>
        calculate_checksum()
}

p1 = solve_disk_problem(disk)
sprintf("Part 1 solution: %s", p1)
```

    ## [1] "Part 1 solution: 6360094256423"

``` r
# part 2
solve_disk_part2 = function(disk_data) {
    disk_space = parse_disk_data(disk_data)
    id_seq = generate_id_sequence(disk_space)
    current_id = max(id_seq)
    empty_positions = which(id_seq == -1L)

    while (current_id > 0L) {
        target_pos = which.max(id_seq == current_id)
        block_size = disk_space[target_pos]
        valid_moves = empty_positions[disk_space[empty_positions] >= block_size & empty_positions < target_pos]

        if (length(valid_moves) > 0L) {
            move_to = valid_moves[1L]
            remaining_space = disk_space[move_to] - block_size
            id_seq[target_pos] = -1L

            disk_space[move_to] = block_size
            id_seq[move_to] = current_id

            if (remaining_space > 0L) {
                disk_space = append(disk_space, remaining_space, after = move_to)
                id_seq = append(id_seq, -1L, after = move_to)
            }

            empty_positions = which(id_seq == -1L)
        }

        current_id = current_id - 1L
    }

    checksum = map_blocks(disk_space, id_seq) |> calculate_checksum()
    checksum
}

p2 = solve_disk_part2(disk)
sprintf("Part 2 solution: %s", p2)
```

    ## [1] "Part 2 solution: 6379677752410"
