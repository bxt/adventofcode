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

fn part1(input: &Vec<Vec<Cell>>) -> i32 {
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
                            if field[y][if x == 1 { field[y].len() - 2 } else { x - 1 }]
                                .contains(&Heading::Right)
                            {
                                blizzards.push(Heading::Right);
                            }
                            if field[y][if x == field[y].len() - 2 { 1 } else { x + 1 }]
                                .contains(&Heading::Left)
                            {
                                blizzards.push(Heading::Left);
                            }
                            if field[if y == 2 { field.len() - 3 } else { y - 1 }][x]
                                .contains(&Heading::Down)
                            {
                                blizzards.push(Heading::Down);
                            }
                            if field[if y == field.len() - 3 { 2 } else { y + 1 }][x]
                                .contains(&Heading::Up)
                            {
                                blizzards.push(Heading::Up);
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
        // dbg!(&potential_expeditions);
        potential_expeditions.retain(|&(x, y)| field[y][x].is_empty());
        expeditions = potential_expeditions;
        // dbg!(field);
        // dbg!(expeditions);
        dbg!(expeditions.len());
        if expeditions
            .iter()
            .find(|(_, y)| *y == field.len() - 2)
            .is_some()
        {
            return round;
        }
    }

    unreachable!()
}

#[test]
fn check_part1() {
    assert_eq!(
        part1(&parse_input(
            &std::fs::read_to_string("day24/example.txt").unwrap()
        )),
        18
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day24/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    // let part2 = part2(&parsed_input);
    // println!("part 2: {}", part2);

    Ok(())
}
