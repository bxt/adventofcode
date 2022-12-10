use std::error::Error;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
enum Instruction {
    AddX(i32),
    Noop,
}

#[derive(Debug, PartialEq, Eq)]
struct ParseInstructionError(String);

impl FromStr for Instruction {
    type Err = ParseInstructionError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        input
            .split_once(" ")
            .map(|(a, b)| {
                if a == "addx" {
                    b.parse()
                        .map(|op| Instruction::AddX(op))
                        .map_err(|e| ParseInstructionError(e.to_string()))
                } else {
                    Err(ParseInstructionError(format!(
                        "not a known 1-op instruction {}",
                        input
                    )))
                }
            })
            .unwrap_or_else(|| {
                if input == "noop" {
                    Ok(Instruction::Noop)
                } else {
                    Err(ParseInstructionError(format!(
                        "not a known 0-op instruction {}",
                        input
                    )))
                }
            })
    }
}

fn parse_input(input: &str) -> Vec<Instruction> {
    input
        .trim()
        .lines()
        .map(|line| line.parse().unwrap())
        .collect::<Vec<_>>()
}

fn run_instructions(input: &Vec<Instruction>) -> Vec<i32> {
    let mut register: i32 = 1;
    let mut result: Vec<i32> = vec![];

    for instruction in input {
        match instruction {
            Instruction::AddX(operand) => {
                result.push(register);
                register += operand;
                result.push(register);
            }
            Instruction::Noop => {
                result.push(register);
            }
        }
    }

    result
}

fn part1(input: &Vec<Instruction>) -> i32 {
    let result = run_instructions(input);
    let check_cycles = [20, 60, 100, 140, 180, 220];
    let mut total_signal_strength = 0;
    // println!("cyc 20: {:?}", result);
    // println!("cyc 20: {:?}", result[20 - 2]);
    // println!("cyc 60: {:?}", result[60 - 2]);
    // println!("cyc 100: {:?}", result[100 - 2]);
    // println!("cyc 140: {:?}", result[140 - 2]);
    // println!("cyc 180: {:?}", result[180 - 2]);
    // println!("cyc 220: {:?}", result[220 - 2]);
    for cycle in check_cycles {
        let result_index = usize::try_from(cycle - 2).unwrap();
        let register_value = result[result_index];
        let signal_strength = cycle * register_value;
        total_signal_strength += signal_strength;
        println!(
            "cyc {:?}: result[{:?}] = {:?} -> {:?} (S {:?})",
            cycle, result_index, register_value, signal_strength, total_signal_strength
        );
    }

    total_signal_strength
}

#[test]
fn check_part1() {
    assert_eq!(
        part1(&parse_input(
            &std::fs::read_to_string("day10/example.txt").unwrap()
        )),
        13140
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day10/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    // let part2 = part2(&parsed_input);
    // println!("part 2: {}", part2);

    Ok(())
}
