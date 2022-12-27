use std::{collections::HashSet, str::FromStr};

use strum_macros::EnumString;

#[derive(Debug, Clone, Copy, PartialEq, EnumString)]
enum Heading {
    #[strum(serialize = ">")]
    Right,
    #[strum(serialize = "v")]
    Down,
    #[strum(serialize = "<")]
    Left,
    #[strum(serialize = "^")]
    Up,
}

#[derive(Debug)]
enum Cell {
    Wall,
    Space(Vec<Heading>),
}

impl Cell {
    fn is_empty(&self) -> bool {
        match self {
            Cell::Space(vec) => vec.len() == 0,
            _ => false,
        }
    }
    fn contains(&self, heading: &Heading) -> bool {
        match self {
            Cell::Space(vec) => vec.contains(heading),
            _ => false,
        }
    }
}

impl FromStr for Cell {
    type Err = std::fmt::Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match input {
            "#" => Ok(Self::Wall),
            "." => Ok(Self::Space(vec![])),
            _ => input
                .parse()
                .map(|h| Self::Space(vec![h]))
                .map_err(|_error| std::fmt::Error),
        }
    }
}

fn parse_input(input: &str) -> Vec<Vec<Cell>> {
    input
        .trim()
        .lines()
        .map(|line| {
            line.split("")
                .filter_map(|letter| letter.parse().ok())
                .collect()
        })
        .collect()
}

fn solve<const FORGOT_SNACKS: bool>(input: &Vec<Vec<Cell>>) -> i32 {
    let mut has_snacks = !FORGOT_SNACKS;
    let mut is_going_back = false;

    let mut field: Vec<Vec<Cell>> = Vec::with_capacity(input.len() + 2);
    field.push(
        (0..input[0].len())
            .map(|_| Cell::Wall)
            .collect::<Vec<Cell>>(),
    );
    field.extend(input.iter().map(|line| {
        line.iter()
            .map(|cell| match cell {
                Cell::Wall => Cell::Wall,
                Cell::Space(vec) => Cell::Space(vec.to_owned()),
            })
            .collect()
    }));
    field.push(
        (0..input[0].len())
            .map(|_| Cell::Wall)
            .collect::<Vec<Cell>>(),
    );

    let mut expeditions = HashSet::new();
    expeditions.insert((field[1].iter().position(|cell| cell.is_empty()).unwrap(), 1));

    for round in 1.. {
        field = field
            .iter()
            .enumerate()
            .map(|(y, line)| {
                line.iter()
                    .enumerate()
                    .map(|(x, cell)| match cell {
                        Cell::Wall => Cell::Wall,
                        Cell::Space(_) => {
                            let mut blizzards = vec![];
                            for (cell, heading) in [
                                (
                                    &field[y][if x == 1 { field[y].len() - 2 } else { x - 1 }],
                                    Heading::Right,
                                ),
                                (
                                    &field[y][if x == field[y].len() - 2 { 1 } else { x + 1 }],
                                    Heading::Left,
                                ),
                                (
                                    &field[if y == 2 { field.len() - 3 } else { y - 1 }][x],
                                    Heading::Down,
                                ),
                                (
                                    &field[if y == field.len() - 3 { 2 } else { y + 1 }][x],
                                    Heading::Up,
                                ),
                            ] {
                                if cell.contains(&heading) {
                                    blizzards.push(heading);
                                }
                            }
                            Cell::Space(blizzards)
                        }
                    })
                    .collect()
            })
            .collect();

        let mut potential_expeditions = HashSet::new();
        for &(x, y) in &expeditions {
            for position in [(x, y), (x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)] {
                potential_expeditions.insert(position);
            }
        }
        potential_expeditions.retain(|&(x, y)| field[y][x].is_empty());
        expeditions = potential_expeditions;

        let target_y = if is_going_back { 1 } else { field.len() - 2 };

        if let Some(&endpoint) = expeditions.iter().find(|(_, y)| *y == target_y) {
            if has_snacks {
                return round;
            } else {
                if is_going_back {
                    has_snacks = true;
                }
                is_going_back = !is_going_back;
                expeditions = HashSet::new();
                expeditions.insert(endpoint);
            }
        }
    }

    unreachable!()
}

#[test]
fn check_part1() {
    assert_eq!(
        solve::<false>(&parse_input(
            &std::fs::read_to_string("day24/example.txt").unwrap()
        )),
        18
    );
}

#[test]
fn check_part2() {
    assert_eq!(
        solve::<true>(&parse_input(
            &std::fs::read_to_string("day24/example.txt").unwrap()
        )),
        54
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day24/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = solve::<false>(&parsed_input);
    println!("part 1: {}", part1);

    let part2 = solve::<true>(&parsed_input);
    println!("part 2: {}", part2);

    Ok(())
}
