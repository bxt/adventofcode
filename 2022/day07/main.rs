use regex::Regex;
use std::collections::HashMap;
use std::fmt::Error;

#[derive(Debug)]
struct CommandOutput {
    command: String,
    lines: Vec<String>,
}

impl std::str::FromStr for CommandOutput {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let (a, b) = input.split_once("\n").unwrap();

        Ok(Self {
            command: a.to_owned(),
            lines: b.lines().map(|s| s.to_owned()).collect(),
        })
    }
}

fn parse_input(input: &str) -> Vec<CommandOutput> {
    input
        .trim()
        .split("$ ")
        .filter(|block| block != &"")
        .map(|block| block.parse().unwrap())
        .collect::<Vec<_>>()
}

fn reconstruct_directories(input: &Vec<CommandOutput>) -> HashMap<String, u32> {
    // TODO: figure out ownership

    let mut sizes: HashMap<String, u32> = HashMap::new();
    let mut current_directory = "".to_owned();

    for command_output in input {
        let command_bits = command_output.command.split(" ").collect::<Vec<_>>();
        match command_bits[..] {
            ["cd", path] => match path {
                ".." => {
                    current_directory = current_directory
                        .trim_end_matches(|c| c != '/')
                        .trim_end_matches('/')
                        .to_owned();
                }
                "/" => {
                    current_directory = "".to_owned();
                }
                name => {
                    current_directory = "".to_owned() + &current_directory + "/" + name;
                }
            },
            ["ls"] => {
                let file_sizes = command_output.lines.iter().filter_map(|line| {
                    let re: Regex = Regex::new(r"^(\d+) ").unwrap();
                    re.captures(line)
                        .and_then(|caps| caps[1].parse::<u32>().ok())
                });
                let size = file_sizes.sum();
                sizes.insert(current_directory.clone(), size);
            }
            _ => {
                panic!("unknown command {}", command_output.command)
            }
        }
    }

    let mut total_sizes: HashMap<String, u32> = HashMap::new();

    for path in (&sizes).keys() {
        let total_size = (&sizes)
            .iter()
            .filter_map(|(child_path, size)| child_path.starts_with(path).then(|| size))
            .sum();
        total_sizes.insert(path.clone(), total_size);
    }

    total_sizes
}

fn part1(input: &Vec<CommandOutput>) -> u32 {
    reconstruct_directories(input)
        .values()
        .filter(|v| v <= &&100000u32)
        .sum()
}

#[test]
fn check_part1() {
    assert_eq!(
        part1(&parse_input(
            &std::fs::read_to_string("day07/example.txt").unwrap()
        )),
        95437
    );
}

fn part2(input: &Vec<CommandOutput>) -> u32 {
    let reconstructed_directories = reconstruct_directories(input);
    let root_size = reconstructed_directories.get("").unwrap();
    let min_size = 30000000 + root_size - 70000000;

    *reconstructed_directories
        .values()
        .filter(|v| v >= &&min_size)
        .min()
        .unwrap()
}

#[test]
fn check_part2() {
    assert_eq!(
        part2(&parse_input(
            &std::fs::read_to_string("day07/example.txt").unwrap()
        )),
        24933642
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day07/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    let part2 = part2(&parsed_input);
    println!("part 2: {}", part2);

    Ok(())
}
