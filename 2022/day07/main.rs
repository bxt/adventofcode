use std::collections::HashSet;
use std::fmt::Error;
use std::rc::Rc;

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

#[derive(Debug)]
struct Directory {
    name: String,
    size: u32,
    children: Vec<Rc<Directory>>,
}

fn reconstruct_directories(input: &Vec<CommandOutput>) -> Directory {
    // TODO: figure out ownership

    let mut root = Rc::new(Directory {
        name: "".to_owned(),
        size: 0,
        children: vec![],
    });
    let mut current_directories = vec![&mut root];

    for command_output in input {
        let command_bits = command_output.command.split(" ").collect::<Vec<_>>();
        match command_bits[..] {
            ["cd", path] => match path {
                ".." => {
                    current_directories.pop();
                }
                "/" => {
                    // current_directories = vec![&mut root];
                }
                name => {
                    let mut newDir =  Rc::new(Directory {
                        name: name.to_owned(),
                        size: 0,
                        children: vec![],
                    });
                    current_directories.last_mut().unwrap().children.push(newDir);
                    current_directories.push(Rc::clone(&newDir));
                }
            },
            ["ls"] => {}
            _ => {
                panic!("unknown command {}", command_output.command)
            }
        }
    }

    root
}

fn part1(input: &Vec<CommandOutput>) -> u32 {
    println!("dirs: {:?}", reconstruct_directories(input));

    95437
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day07/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    Ok(())
}
