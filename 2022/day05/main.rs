use regex::Regex;
use std::fmt::Error;

#[derive(Debug, Clone)]
struct Crates {
    stacks: Vec<Vec<char>>,
}

impl Crates {
    fn lift(&mut self, from: usize, to: usize) {
        let c = self.stacks[from].pop().unwrap();
        self.stacks[to].push(c);
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
        let mut stacks = vec![];
        for _x in 0..stack_count {
            stacks.push(vec![]);
        }
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
    let (crates_str, instructions_str) = input.trim().split_once("\n\n").unwrap();
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
        for _x in 0..instruction.repetitions {
            crates.lift(instruction.from - 1, instruction.to - 1);
        }
    }

    crates.tops()
}

const EXAMPLE: &str = "\
        [D]    \n\
    [N] [C]    \n\
    [Z] [M] [P]\n\
     1   2   3 \n\
    \n\
    move 1 from 2 to 1\n\
    move 3 from 1 to 3\n\
    move 2 from 2 to 1\n\
    move 1 from 1 to 2\
    ";

// #[test]
// fn check_part1() {
//     assert_eq!(part1(&parse_input(EXAMPLE)), 2);
// }

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day05/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(parsed_input);
    println!("part 1: {:?}", part1);

    // let part2 = part2(&parsed_input);
    // println!("part 2: {}", part2);

    Ok(())
}
