use std::collections::HashMap;

fn tilt_north(width: usize, height: usize, lines: Vec<&str>, rolling_rocks: &mut Vec<u64>) {
    for x in 0..width {
        let mut next_free_spot = 0;
        for y in 0..height {
            if lines[y].bytes().nth(x).unwrap() == b'#' {
                next_free_spot = y + 1;
            } else {
                let index = y * width + x;
                let rolling_rocks_index = index / 64;
                let bytes_index = index % 64;
                if (rolling_rocks[rolling_rocks_index] & (1 << bytes_index)) > 0 {
                    if next_free_spot != y {
                        rolling_rocks[rolling_rocks_index] &= !(1 << bytes_index);

                        let index = next_free_spot * width + x;
                        let rolling_rocks_index = index / 64;
                        let bytes_index = index % 64;
                        rolling_rocks[rolling_rocks_index] |= 1 << bytes_index;
                    }
                    next_free_spot += 1;
                }
            }
        }
    }
}

fn sum_north_support_beam_load(height: usize, width: usize, rolling_rocks: &Vec<u64>) -> usize {
    let mut north_support_beam_load = 0;

    for y in 0..height {
        for x in 0..width {
            let index = y * width + x;
            let rolling_rocks_index = index / 64;
            let bytes_index = index % 64;
            if (rolling_rocks[rolling_rocks_index] & (1 << bytes_index)) > 0 {
                north_support_beam_load += height - y;
            }
        }
    }
    north_support_beam_load
}

fn main() -> () {
    let file = std::fs::read_to_string("day14/input.txt").unwrap();

    let lines = file
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .collect::<Vec<_>>();

    // let mut seen = HashMap::new();

    let width = lines[0].len();
    let height = lines.len();

    let mut rolling_rocks: Vec<u64> = vec![];

    for (y, line) in lines.iter().enumerate() {
        for (x, byte) in line.bytes().enumerate() {
            let index = y * width + x;
            let rolling_rocks_index = index / 64;
            let bytes_index = index % 64;
            if bytes_index == 0 {
                rolling_rocks.push(0);
            }
            if byte == b'O' {
                rolling_rocks[rolling_rocks_index] |= 1 << bytes_index;
            }
        }
    }

    tilt_north(width, height, lines, &mut rolling_rocks);

    let part1 = sum_north_support_beam_load(height, width, &rolling_rocks);
    println!("Part 1: {:?}", part1);
}
