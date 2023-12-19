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
            "U" => Ok(Direction::U),
            "L" => Ok(Direction::L),
            "D" => Ok(Direction::D),
            "R" => Ok(Direction::R),
            _ => Err("Not a valid direction: ".to_string() + s),
        }
    }
}

fn is_counterclockwise(from: Direction, to: Direction) -> bool {
    let all_directions = [Direction::U, Direction::L, Direction::D, Direction::R];
    let direction_index = all_directions.iter().position(|&d| d == from).unwrap();
    to == all_directions[(direction_index + 1) % all_directions.len()]
}

#[test]
fn check_is_counterclockwise() {
    assert_eq!(is_counterclockwise(Direction::L, Direction::D), true);
    assert_eq!(is_counterclockwise(Direction::D, Direction::R), true);
    assert_eq!(is_counterclockwise(Direction::D, Direction::L), false);
    assert_eq!(is_counterclockwise(Direction::R, Direction::D), false);
}

fn main() -> () {
    let file = std::fs::read_to_string("day18/input.txt").unwrap();

    let instructions = file
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .map(|line| {
            let (direction_str, rest) = line.split_once(" ").unwrap();
            let (distance_str, _) = rest.split_once(" ").unwrap();
            let direction: Direction = direction_str.parse().unwrap();
            let distance: isize = distance_str.parse().unwrap();
            (direction, distance)
        })
        .collect::<Vec<_>>();

    let mut x_position = 0;
    let mut lagoon_size = 0;

    for index in 0..instructions.len() {
        let (direction, distance) = instructions[index];
        let (prev_direction, _) =
            instructions[(index + instructions.len() - 1) % instructions.len()];
        let (next_direction, _) = instructions[(index + 1) % instructions.len()];
        let outer_distance = distance
            + isize::from(!is_counterclockwise(prev_direction, direction))
            - isize::from(is_counterclockwise(direction, next_direction));
        match direction {
            Direction::U => lagoon_size += outer_distance * x_position,
            Direction::L => x_position += outer_distance,
            Direction::D => lagoon_size -= outer_distance * x_position,
            Direction::R => x_position -= outer_distance,
        }
    }

    println!("Part 1: {:?}", lagoon_size);
}
