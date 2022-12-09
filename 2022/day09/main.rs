use std::collections::HashSet;
use std::fmt::Error;
use std::str::FromStr;
use strum_macros::{EnumIter, EnumString};

#[derive(Debug, EnumString, EnumIter)]
enum Direction {
    L,
    R,
    U,
    D,
}

#[derive(Debug)]
struct Move {
    direction: Direction,
    amount: u32,
}

impl FromStr for Move {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let (a, b) = input.split_once(" ").unwrap();

        Ok(Self {
            direction: a.parse().unwrap(),
            amount: b.parse().unwrap(),
        })
    }
}

fn parse_input(input: &str) -> Vec<Move> {
    input
        .trim()
        .lines()
        .map(|line| line.parse().unwrap())
        .collect::<Vec<_>>()
}

fn visit_places(input: &Vec<Move>, chain_length: usize) -> HashSet<(i32, i32)> {
    let mut visited: HashSet<(i32, i32)> = HashSet::new();
    let mut chain: Vec<(i32, i32)> = vec![(0, 0); chain_length];

    for Move { direction, amount } in input {
        for _ in 0..*amount {
            match direction {
                Direction::L => {
                    chain[0] = (chain[0].0 - 1, chain[0].1);
                }
                Direction::R => {
                    chain[0] = (chain[0].0 + 1, chain[0].1);
                }
                Direction::U => {
                    chain[0] = (chain[0].0, chain[0].1 - 1);
                }
                Direction::D => {
                    chain[0] = (chain[0].0, chain[0].1 + 1);
                }
            }

            for index in 1..chain.len() {
                let (head_x, head_y) = chain[index - 1];
                let (tail_x, tail_y) = &mut chain[index];

                let tail_diff_x = head_x - *tail_x;
                let tail_diff_y = head_y - *tail_y;
                if tail_diff_x.abs() == 2 {
                    if tail_diff_y.abs() == 2 {
                        *tail_x += tail_diff_x / 2;
                        *tail_y += tail_diff_y / 2;
                    } else {
                        *tail_x += tail_diff_x / 2;
                        *tail_y += tail_diff_y;
                    }
                } else if tail_diff_y.abs() == 2 {
                    *tail_x += tail_diff_x;
                    *tail_y += tail_diff_y / 2;
                }
            }

            visited.insert(*chain.last().unwrap());
        }
    }

    visited
}

fn part1(input: &Vec<Move>) -> usize {
    visit_places(input, 2).len()
}

#[test]
fn check_part1() {
    assert_eq!(
        part1(&parse_input(
            &std::fs::read_to_string("day09/example.txt").unwrap()
        )),
        13
    );
}

fn part2(input: &Vec<Move>) -> usize {
    visit_places(input, 10).len()
}

#[test]
fn check_part2() {
    assert_eq!(
        part2(&parse_input(
            &std::fs::read_to_string("day09/example.txt").unwrap()
        )),
        1
    );
}

#[test]
fn check_part2_large() {
    assert_eq!(
        part2(&parse_input(
            &std::fs::read_to_string("day09/example-large.txt").unwrap()
        )),
        36
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day09/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    let part2 = part2(&parsed_input);
    println!("part 2: {}", part2);

    Ok(())
}
