use std::{collections::HashMap, iter::repeat};

struct BitField {
    store: Vec<u64>,
    width: usize,
}

impl BitField {
    fn new(width: usize, height: usize) -> Self {
        let mut bit_field = BitField {
            store: Vec::new(),
            width,
        };

        let (store_index, _) = bit_field.transform_index(width - 1, height - 1);
        let store_size = store_index + 1;
        bit_field.store.extend(repeat(0).take(store_size));

        bit_field
    }

    fn transform_index(&self, x: usize, y: usize) -> (usize, usize) {
        let index = y * self.width + x;
        let store_index = index / 64;
        let bytes_index = index % 64;
        (store_index, bytes_index)
    }

    fn is_set(&self, x: usize, y: usize) -> bool {
        let (store_index, bytes_index) = self.transform_index(x, y);
        self.store[store_index] & (1 << bytes_index) > 0
    }

    fn set(&mut self, x: usize, y: usize) {
        let (store_index, bytes_index) = self.transform_index(x, y);
        self.store[store_index] |= 1 << bytes_index;
    }

    fn unset(&mut self, x: usize, y: usize) {
        let (store_index, bytes_index) = self.transform_index(x, y);
        self.store[store_index] &= !(1 << bytes_index);
    }
}

fn tilt_north(width: usize, height: usize, lines: &Vec<&str>, rolling_rocks: &mut BitField) {
    for x in 0..width {
        let mut next_free_spot = 0;
        for y in 0..height {
            if lines[y].bytes().nth(x).unwrap() == b'#' {
                next_free_spot = y + 1;
            } else {
                if rolling_rocks.is_set(x, y) {
                    if next_free_spot != y {
                        rolling_rocks.unset(x, y);
                        rolling_rocks.set(x, next_free_spot);
                    }
                    next_free_spot += 1;
                }
            }
        }
    }
}

fn tilt_west(width: usize, height: usize, lines: &Vec<&str>, rolling_rocks: &mut BitField) {
    for y in 0..height {
        let mut next_free_spot = 0;
        for x in 0..width {
            if lines[y].bytes().nth(x).unwrap() == b'#' {
                next_free_spot = x + 1;
            } else {
                if rolling_rocks.is_set(x, y) {
                    if next_free_spot != x {
                        rolling_rocks.unset(x, y);
                        rolling_rocks.set(next_free_spot, y);
                    }
                    next_free_spot += 1;
                }
            }
        }
    }
}

fn tilt_south(width: usize, height: usize, lines: &Vec<&str>, rolling_rocks: &mut BitField) {
    for x in 0..width {
        let mut next_free_spot = height - 1;
        for y in (0..height).rev() {
            if lines[y].bytes().nth(x).unwrap() == b'#' {
                if y != 0 {
                    next_free_spot = y - 1;
                }
            } else {
                if rolling_rocks.is_set(x, y) {
                    if next_free_spot != y {
                        rolling_rocks.unset(x, y);
                        rolling_rocks.set(x, next_free_spot);
                    }
                    if next_free_spot != 0 {
                        next_free_spot -= 1;
                    } else {
                        if y != 0 {
                            panic!("Yikes!");
                        }
                    }
                }
            }
        }
    }
}

fn tilt_east(width: usize, height: usize, lines: &Vec<&str>, rolling_rocks: &mut BitField) {
    for y in 0..height {
        let mut next_free_spot = width - 1;
        for x in (0..width).rev() {
            if lines[y].bytes().nth(x).unwrap() == b'#' {
                if x != 0 {
                    next_free_spot = x - 1;
                }
            } else {
                if rolling_rocks.is_set(x, y) {
                    if next_free_spot != x {
                        rolling_rocks.unset(x, y);
                        rolling_rocks.set(next_free_spot, y);
                    }
                    if next_free_spot != 0 {
                        next_free_spot -= 1;
                    } else {
                        if x != 0 {
                            panic!("Yikes!");
                        }
                    }
                }
            }
        }
    }
}

fn sum_north_support_beam_load(height: usize, width: usize, rolling_rocks: &BitField) -> usize {
    let mut north_support_beam_load = 0;

    for y in 0..height {
        for x in 0..width {
            if rolling_rocks.is_set(x, y) {
                north_support_beam_load += height - y;
            }
        }
    }
    north_support_beam_load
}

fn main() -> () {
    let file = std::fs::read_to_string("day14/input.txt").unwrap();
    // let file = std::fs::read_to_string("day14/example.txt").unwrap();

    let lines = file
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .collect::<Vec<_>>();

    let mut seen = HashMap::new();

    let width = lines[0].len();
    let height = lines.len();

    let mut rolling_rocks = BitField::new(width, height);

    for (y, line) in lines.iter().enumerate() {
        for (x, byte) in line.bytes().enumerate() {
            if byte == b'O' {
                rolling_rocks.set(x, y);
            }
        }
    }

    tilt_north(width, height, &lines, &mut rolling_rocks);

    let part1 = sum_north_support_beam_load(height, width, &rolling_rocks);
    println!("Part 1: {:?}", part1);

    tilt_west(width, height, &lines, &mut rolling_rocks);
    tilt_south(width, height, &lines, &mut rolling_rocks);
    tilt_east(width, height, &lines, &mut rolling_rocks);

    let mut cycles_done = 1;
    let cycles_target = 1000000000;

    while cycles_done != cycles_target {
        tilt_north(width, height, &lines, &mut rolling_rocks);
        tilt_west(width, height, &lines, &mut rolling_rocks);
        tilt_south(width, height, &lines, &mut rolling_rocks);
        tilt_east(width, height, &lines, &mut rolling_rocks);
        cycles_done += 1;

        if let Some(previous_cycles_done) = seen.get(&rolling_rocks.store) {
            let loop_size = cycles_done - previous_cycles_done;
            let cycles_left = cycles_target - cycles_done;
            let skip_loops = cycles_left / loop_size;
            let skip_cycles = skip_loops * loop_size;
            cycles_done += skip_cycles;
        } else {
            seen.insert(rolling_rocks.store.to_vec(), cycles_done);
        }
    }

    let part2 = sum_north_support_beam_load(height, width, &rolling_rocks);
    println!("Part 2: {:?}", part2);
}
