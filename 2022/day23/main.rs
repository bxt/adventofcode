use std::cmp::{max, min};
use std::collections::HashMap;
use std::vec;

fn parse_input(input: &str) -> Vec<(i32, i32)> {
    input
        .split("\n")
        .enumerate()
        .flat_map(|(y, line)| {
            line.bytes().enumerate().filter_map(move |(x, byte)| {
                (byte == b'#').then_some((i32::try_from(x).unwrap(), i32::try_from(y).unwrap()))
            })
        })
        .collect()
}

fn figure_dimensions(field: &Vec<(i32, i32)>) -> ((i32, i32), (i32, i32)) {
    let mut dimensions: ((i32, i32), (i32, i32)) = (field[0], field[0]);

    for pos in field {
        dimensions.0 .0 = min(dimensions.0 .0, pos.0);
        dimensions.0 .1 = max(dimensions.0 .1, pos.0);
        dimensions.1 .0 = min(dimensions.1 .0, pos.1);
        dimensions.1 .1 = max(dimensions.1 .1, pos.1);
    }

    dimensions
}

fn neighbours((x, y): (i32, i32)) -> [(i32, i32); 8] {
    [
        (x - 1, y - 1),
        (x + 0, y - 1), // N
        (x + 1, y - 1),
        (x + 1, y + 0), // E
        (x + 1, y + 1),
        (x + 0, y + 1), // S
        (x - 1, y + 1),
        (x - 1, y + 0), // W
    ]
}

fn part1(input: &Vec<(i32, i32)>) -> u32 {
    let mut field = input.to_vec();

    for round in 0..10 {
        let nswe: [usize; 4] = [1, 5, 7, 3];

        let mut new_field = vec![];

        for &position in field.iter() {
            let neighbours = neighbours(position);
            let present_neighbours = neighbours.map(|n| field.contains(&n));
            let new_position = if present_neighbours.iter().filter(|&&b| b).count() == 0 {
                position
            } else {
                (0..nswe.len())
                    .find_map(|i| {
                        let direction_index = nswe[(round + i) % 4];
                        ([direction_index - 1, direction_index, direction_index + 1]
                            .iter()
                            .filter(|&&index| present_neighbours[index % present_neighbours.len()])
                            .count()
                            == 0)
                            .then_some(neighbours[direction_index])
                    })
                    .unwrap_or(position)
            };
            new_field.push(new_position);
        }

        let mut frequencies: HashMap<(i32, i32), usize> = HashMap::new();
        for &position in new_field.iter() {
            *frequencies.entry(position).or_default() += 1;
        }

        for (index, _) in new_field.iter().enumerate() {
            let new_position = new_field[index];
            if frequencies[&new_position] == 1 {
                field[index] = new_position
            }
        }
    }

    let dimensions = figure_dimensions(&field);
    let area = (dimensions.0 .0.abs_diff(dimensions.0 .1) + 1)
        * (dimensions.1 .0.abs_diff(dimensions.1 .1) + 1);
    let occupied = u32::try_from(field.len()).unwrap();
    let free = area - occupied;
    free
}

fn main() {
    let file = std::fs::read_to_string("day23/input.txt").unwrap();

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    // let part2 = part2(&parsed_input);
    // println!("part 2: {}", part2);
}
