use regex::Regex;
use std::{collections::VecDeque, str::FromStr};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Debug, PartialEq, EnumIter)]
enum Operation {
    Add(u64),
    Times(u64),
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
    fn apply(&self, old: u64) -> u64 {
        match self {
            Operation::Add(immediate_operand) => old + immediate_operand,
            Operation::Times(immediate_operand) => old * immediate_operand,
            Operation::Square => old * old,
        }
    }
}

#[derive(Debug)]
struct Monkey {
    initial_items: Vec<u64>,
    operation: Operation,
    test_divisor: u64,
    monkey_true_index: usize,
    monkey_false_index: usize,
}

impl FromStr for Monkey {
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

impl Monkey {
    fn inspect(&self, item: u64, monkey_modulus: u64, devide_by_three: bool) -> (usize, u64) {
        let mut new_item = self.operation.apply(item);
        if devide_by_three {
            new_item /= 3;
        }
        new_item %= monkey_modulus;
        (
            match new_item % self.test_divisor == 0 {
                true => self.monkey_true_index,
                false => self.monkey_false_index,
            },
            new_item,
        )
    }
}

fn parse_input(input: &str) -> Vec<Monkey> {
    input
        .split("\n\n")
        .map(|monkey_str| monkey_str.parse().unwrap())
        .collect()
}

fn run_monkey_business(input: &Vec<Monkey>, devide_by_three: bool, rounds: u32) -> u64 {
    let monkey_modulus: u64 = input.iter().map(|monkey| monkey.test_divisor).product();

    let mut monkey_items = input
        .iter()
        .map(|monkey| monkey.initial_items.to_vec().into_iter().collect())
        .collect::<Vec<VecDeque<u64>>>();
    let mut monkey_inspect_counts = vec![0; input.len()];

    for _ in 1..=rounds {
        for monkey_index in 0..monkey_items.len() {
            while let Some(item) = monkey_items[monkey_index].pop_front() {
                monkey_inspect_counts[monkey_index] += 1;
                let (new_index, new_item) =
                    input[monkey_index].inspect(item, monkey_modulus, devide_by_three);
                monkey_items[new_index].push_back(new_item);
            }
        }
    }

    {
        let mut mic = monkey_inspect_counts;
        mic.sort();
        mic[mic.len() - 1] * mic[mic.len() - 2]
    }
}

fn part1(input: &Vec<Monkey>) -> u64 {
    run_monkey_business(input, true, 20)
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

fn part2(input: &Vec<Monkey>) -> u64 {
    run_monkey_business(input, false, 10000)
}

#[test]
fn check_part2() {
    assert_eq!(
        part2(&parse_input(
            &std::fs::read_to_string("day11/example.txt").unwrap()
        )),
        2713310158
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day11/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    let part2 = part2(&parsed_input);
    println!("part 2: {}", part2);

    Ok(())
}
