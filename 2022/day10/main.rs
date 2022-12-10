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
            .map(|(op_code, operand)| match op_code {
                "addx" => operand
                    .parse()
                    .map(Instruction::AddX)
                    .map_err(|e| ParseInstructionError(e.to_string())),
                other => Err(ParseInstructionError(format!(
                    "not a known 1-op instruction {}",
                    other
                ))),
            })
            .unwrap_or_else(|| {
                (input == "noop")
                    .then_some(Instruction::Noop)
                    .ok_or(ParseInstructionError(format!(
                        "not a known 0-op instruction {}",
                        input
                    )))
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
    let mut result: Vec<i32> = vec![1];

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

    for cycle in check_cycles {
        let result_index = usize::try_from(cycle - 1).unwrap();
        let register_value = result[result_index];
        let signal_strength = cycle * register_value;
        total_signal_strength += signal_strength;
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

fn part2(input: &Vec<Instruction>) {
    let result = run_instructions(input);
    let mut screen = [false; 40 * 6];

    for (cycle, register_value) in result.into_iter().enumerate() {
        let horizontal_draw_position = i32::try_from(cycle % 40).unwrap();
        let is_sprite_there = horizontal_draw_position.abs_diff(register_value) < 2;
        if is_sprite_there {
            screen[cycle] = true;
        }
    }

    for y in 0..6 {
        for x in 0..40 {
            print!("{}", if screen[y * 40 + x] { "#" } else { "." })
        }
        println!("")
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day10/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    println!("part 2:");
    part2(&parsed_input);

    Ok(())
}
