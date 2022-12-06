use regex::Regex;
use std::fmt::Error;

#[derive(Debug, Clone)]
struct Crates {
    stacks: Vec<Vec<char>>,
}

impl Crates {
    fn lift(&mut self, instruction: Instruction) {
        for _x in 0..instruction.repetitions {
            let c = self.stacks[instruction.from - 1].pop().unwrap();
            self.stacks[instruction.to - 1].push(c);
        }
    }

    fn lift9001(&mut self, instruction: Instruction) {
        let from_stack = &mut self.stacks[instruction.from - 1];
        let amount = usize::try_from(instruction.repetitions).unwrap();
        let mut crates = from_stack.split_off(from_stack.len() - amount);
        self.stacks[instruction.to - 1].append(&mut crates);
    }

    fn tops(&self) -> String {
        self.stacks
            .iter()
            .map(|stack| stack.last().unwrap())
            .collect()
    }
}

impl std::str::FromStr for Crates {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut lines = input.lines().rev();
        let stack_count = (lines.next().unwrap().len() + 1) / 4;
        let mut stacks = vec![Vec::new(); stack_count];
        for line in lines {
            for stack_number in 0..stack_count {
                let character = line.chars().nth(stack_number * 4 + 1).unwrap();
                if character != ' ' {
                    stacks[stack_number].push(character);
                }
            }
        }

        Ok(Self { stacks: stacks })
    }
}

#[derive(Debug)]
struct Instruction {
    from: usize,
    to: usize,
    repetitions: u32,
}

impl std::str::FromStr for Instruction {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let re: Regex = Regex::new(r"^move (\d+) from (\d+) to (\d+)$").unwrap();
        let caps = re.captures(input).unwrap();

        Ok(Self {
            from: caps[2].parse().unwrap(),
            to: caps[3].parse().unwrap(),
            repetitions: caps[1].parse().unwrap(),
        })
    }
}

fn parse_input(input: &str) -> (Crates, Vec<Instruction>) {
    let (crates_str, instructions_str) = input.split_once("\n\n").unwrap();
    (
        crates_str.parse().unwrap(),
        instructions_str
            .lines()
            .map(|line| line.parse().unwrap())
            .collect(),
    )
}

fn part1((input_crates, instructions): (Crates, Vec<Instruction>)) -> String {
    let mut crates = input_crates;

    for instruction in instructions {
        crates.lift(instruction);
    }

    crates.tops()
}

#[test]
fn check_part1() {
    assert_eq!(
        part1(parse_input(
            &std::fs::read_to_string("day05/example.txt").unwrap()
        )),
        "CMZ"
    );
}

fn part2((input_crates, instructions): (Crates, Vec<Instruction>)) -> String {
    let mut crates = input_crates;

    for instruction in instructions {
        crates.lift9001(instruction);
    }

    crates.tops()
}

#[test]
fn check_part2() {
    assert_eq!(
        part2(parse_input(
            &std::fs::read_to_string("day05/example.txt").unwrap()
        )),
        "MCD"
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day05/input.txt")?;

    let parsed_input = parse_input(&file);
    let part1 = part1(parsed_input);
    println!("part 1: {}", part1);

    let parsed_input2 = parse_input(&file);
    let part2 = part2(parsed_input2);
    println!("part 2: {}", part2);

    Ok(())
}
