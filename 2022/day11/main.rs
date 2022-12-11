// Monkey 0:
// Starting items: 89, 74
// Operation: new = old * 5
// Test: divisible by 17
//   If true: throw to monkey 4
//   If false: throw to monkey 7

use regex::Regex;
use std::{collections::VecDeque, str::FromStr};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Debug, PartialEq, EnumIter)]
enum Operation {
    Add(u32),
    Times(u32),
    Square,
}

impl FromStr for Operation {
    type Err = std::fmt::Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Operation::iter()
            .filter_map(|e| match e {
                Operation::Add(_) => {
                    let re: Regex = Regex::new(r"^old \+ (\d+)$").unwrap();
                    let caps = re.captures(input);
                    caps.map(|c| Operation::Add(c[1].parse().unwrap()))
                }
                Operation::Times(_) => {
                    let re: Regex = Regex::new(r"^old \* (\d+)$").unwrap();
                    let caps = re.captures(input);
                    println!("#######**##### {:?} from {:?}", caps, input);
                    caps.map(|c| Operation::Times(c[1].parse().unwrap()))
                }
                Operation::Square => {
                    let re: Regex = Regex::new(r"^old \* old$").unwrap();
                    let caps = re.captures(input);
                    caps.map(|_| Operation::Square)
                }
            })
            .next()
            .ok_or(std::fmt::Error)
    }
}

impl Operation {
    fn apply(&self, old: u32) -> u32 {
        match self {
            Operation::Add(io) => old + io,
            Operation::Times(io) => old * io,
            Operation::Square => old * old,
        }
    }
}

#[derive(Debug)]

struct MonkeyCharacteristics {
    initial_items: Vec<u32>,
    operation: Operation,
    test_divisor: u32,
    monkey_true_index: usize,
    monkey_false_index: usize,
}

impl FromStr for MonkeyCharacteristics {
    type Err = std::fmt::Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let re: Regex = Regex::new(r"Monkey \d+:\n +Starting items: ([^\n]+)\n +Operation: new = ([^\n]+)\n +Test: divisible by (\d+)\n +If true: throw to monkey (\d+)\n +If false: throw to monkey (\d+)").unwrap();
        let caps = re.captures(input).unwrap();

        Ok(Self {
            initial_items: caps[1].split(", ").map(|i| i.parse().unwrap()).collect(),
            operation: caps[2].parse().unwrap(),
            test_divisor: caps[3].parse().unwrap(),
            monkey_true_index: caps[4].parse().unwrap(),
            monkey_false_index: caps[5].parse().unwrap(),
        })
    }
}

impl MonkeyCharacteristics {
    fn inspect(&self, item: u32) -> (usize, u32) {
        let with_op = self.operation.apply(item);
        let devided = with_op / 3;
        let test_result = devided % self.test_divisor == 0;
        (
            match test_result {
                true => self.monkey_true_index,
                false => self.monkey_false_index,
            },
            devided,
        )
    }
}

#[derive(Debug)]

struct Monkey<'a> {
    characteristics: &'a MonkeyCharacteristics,
    items: VecDeque<u32>,
}

fn parse_input(input: &str) -> Vec<MonkeyCharacteristics> {
    input
        .split("\n\n")
        .map(|monkey_str| monkey_str.parse().unwrap())
        .collect()
}

fn part1(input: &Vec<MonkeyCharacteristics>) -> u32 {
    println!("{:?}", input);

    let mut monkey_items = input
        .iter()
        .map(|characteristics| characteristics.initial_items.to_vec().into_iter().collect())
        .collect::<Vec<VecDeque<u32>>>();
    let mut monkey_inspect_counts = input.iter().map(|_| 0).collect::<Vec<u32>>();

    for round in 1..=20 {
        for monkey_index in 0..monkey_items.len() {
            loop {
                match monkey_items[monkey_index].pop_front() {
                    None => {
                        break;
                    }
                    Some(item) => {
                        monkey_inspect_counts[monkey_index] += 1;
                        let (new_index, new_item) = input[monkey_index].inspect(item);
                        println!(
                            "Item with worry level {} is thrown to monkey {}.",
                            new_item, new_index
                        );
                        monkey_items[new_index].push_back(new_item);
                    }
                }
            }
        }

        println!(
            "After round {}, the monkeys are holding items with these worry levels:",
            round
        );
        for (idx, items) in monkey_items.iter().enumerate() {
            println!("Monkey {}: {:?}", idx, items);
        }
    }
    println!("Tally: {:?}", monkey_inspect_counts);

    monkey_inspect_counts.sort();

    monkey_inspect_counts[monkey_inspect_counts.len() - 1]
        * monkey_inspect_counts[monkey_inspect_counts.len() - 2]
}

#[test]
fn check_part1() {
    assert_eq!(
        part1(&parse_input(
            &std::fs::read_to_string("day11/example.txt").unwrap()
        )),
        10605
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day11/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    Ok(())
}
