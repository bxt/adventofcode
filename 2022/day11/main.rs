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
    name: u8,
    initial_items: Vec<u32>,
    operation: Operation,
    test_divisor: u32,
    monkey_true_name: u8,
    monkey_false_name: u8,
}

impl FromStr for MonkeyCharacteristics {
    type Err = std::fmt::Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let re: Regex = Regex::new(r"Monkey (\d+):\n +Starting items: ([^\n]+)\n +Operation: new = ([^\n]+)\n +Test: divisible by (\d+)\n +If true: throw to monkey (\d+)\n +If false: throw to monkey (\d+)").unwrap();
        let caps = re.captures(input).unwrap();

        Ok(Self {
            name: caps[1].parse().unwrap(),
            initial_items: caps[2].split(", ").map(|i| i.parse().unwrap()).collect(),
            operation: caps[3].parse().unwrap(),
            test_divisor: caps[4].parse().unwrap(),
            monkey_true_name: caps[5].parse().unwrap(),
            monkey_false_name: caps[6].parse().unwrap(),
        })
    }
}

#[derive(Debug)]

struct Monkey<'a> {
    characteristics: &'a MonkeyCharacteristics,
    items: VecDeque<u32>,
}

impl Monkey<'_> {
    fn inspect_next(&mut self) -> Option<(u8, u32)> {
        self.items.pop_front().map(|item| {
            let with_op = self.characteristics.operation.apply(item);
            let devided = with_op / 3;
            let test_result = devided % self.characteristics.test_divisor == 0;
            (
                match test_result {
                    true => self.characteristics.monkey_true_name,
                    false => self.characteristics.monkey_false_name,
                },
                devided,
            )
        })
    }
    fn catch(&mut self, item: u32) {
        self.items.push_back(item);
    }
}

fn parse_input(input: &str) -> Vec<MonkeyCharacteristics> {
    input
        .split("\n\n")
        .map(|monkey_str| monkey_str.parse().unwrap())
        .collect()
}

fn part1(input: &Vec<MonkeyCharacteristics>) -> u32 {
    println!("{:?}", input);

    let mut monkeys = input
        .iter()
        .map(|characteristics| Monkey {
            characteristics,
            items: characteristics.initial_items.to_vec().into_iter().collect(),
        })
        .collect::<Vec<_>>();

    for round in 1..=20 {
        let mut throws = VecDeque::new();
        for monkey in &mut monkeys {
            loop {
                match monkey.inspect_next() {
                    None => {
                        break;
                    }
                    Some(throw) => throws.push_back(throw),
                }
            }
            loop {
                match throws.pop_front() {
                    None => {
                        break;
                    }
                    Some((name, item)) => {
                        println!(
                            "Item with worry level {} is thrown to monkey {}.",
                            item, name
                        );
                        for monkey in &mut monkeys {
                            if monkey.characteristics.name == name {
                                monkey.catch(item)
                            }
                        }
                    }
                }
            }
        }

        println!(
            "After round {}, the monkeys are holding items with these worry levels:",
            round
        );
        for monkey in monkeys.iter() {
            println!("Monkey {}: {:?}", monkey.characteristics.name, monkey.items);
        }
    }

    10605
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
