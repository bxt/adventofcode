use std::str::FromStr;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Direction {
    U,
    L,
    D,
    R,
}

impl FromStr for Direction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "U" | "3" => Ok(Direction::U),
            "L" | "2" => Ok(Direction::L),
            "D" | "1" => Ok(Direction::D),
            "R" | "0" => Ok(Direction::R),
            _ => Err("Not a valid direction: ".to_string() + s),
        }
    }
}

fn get_lagoon_size(instructions: &Vec<(Direction, isize)>) -> isize {
    let mut x_position = 0;
    let mut lagoon_size = 0;
    let mut trench_size = 0;

    for (direction, distance) in instructions {
        trench_size += distance;
        match direction {
            Direction::U => lagoon_size += distance * x_position,
            Direction::L => x_position += distance,
            Direction::D => lagoon_size -= distance * x_position,
            Direction::R => x_position -= distance,
        }
    }
    lagoon_size + trench_size / 2 + 1
}

fn main() -> () {
    let file = std::fs::read_to_string("day18/input.txt").unwrap();

    let instructions_part1 = &file
        .lines()
        .map(|line| {
            let (direction_str, rest) = line.split_once(" ").unwrap();
            let (distance_str, _) = rest.split_once(" ").unwrap();
            let direction: Direction = direction_str.parse().unwrap();
            let distance: isize = distance_str.parse().unwrap();
            (direction, distance)
        })
        .collect::<Vec<_>>();

    println!("Part 1: {:?}", get_lagoon_size(instructions_part1));

    let instructions_part2 = &file
        .lines()
        .map(|line| {
            let (_, hex_code) = line.split_once(" (#").unwrap();
            let distance_str = &hex_code[0..5];
            let direction_str = &hex_code[5..6];
            let direction: Direction = direction_str.parse().unwrap();
            let distance = isize::from_str_radix(distance_str, 16).unwrap();
            (direction, distance)
        })
        .collect::<Vec<_>>();

    println!("Part 2: {:?}", get_lagoon_size(instructions_part2));
}
