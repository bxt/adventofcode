use regex::Regex;
use std::collections::HashSet;
use std::fmt::Error;
use std::str::FromStr;
use strum::IntoEnumIterator;
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

impl std::str::FromStr for Move {
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

fn visit_places(input: &Vec<Move>) -> HashSet<(i32, i32)> {
    // TODO: figure out ownership

    let mut visited: HashSet<(i32, i32)> = HashSet::new();
    let mut head_x = 0i32;
    let mut head_y = 0i32;
    let mut tail_x = 0i32;
    let mut tail_y = 0i32;

    for Move { direction, amount } in input {
        for _ in 0..*amount {
            match direction {
                Direction::L => {
                    head_x -= 1;
                }
                Direction::R => {
                    head_x += 1;
                }
                Direction::U => {
                    head_y -= 1;
                }
                Direction::D => {
                    head_y += 1;
                }
            }
            let tail_diff_x = head_x - tail_x;
            let tail_diff_y = head_y - tail_y;
            if tail_diff_x.abs() == 2 {
                tail_x += tail_diff_x / 2;
                tail_y += tail_diff_y;
            }
            if tail_diff_y.abs() == 2 {
                tail_x += tail_diff_x;
                tail_y += tail_diff_y / 2;
            }
            // println!("H: {}, {}", head_x, head_y);
            // println!("T: {}, {}", tail_x, tail_y);
            // println!("--------");

            visited.insert((tail_x, tail_y));
        }
    }

    visited
}

fn part1(input: &Vec<Move>) -> usize {
    visit_places(input).len()
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day09/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    // let part2 = part2(&parsed_input);
    // println!("part 2: {}", part2);

    Ok(())
}
