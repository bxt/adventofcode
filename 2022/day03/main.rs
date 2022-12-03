use std::collections::HashSet;
use std::iter::FromIterator;

fn parse_input(input: &str) -> Vec<(&str, &str)> {
    input.trim()
    .split("\n")
    .flat_map(|line| parse_line(line)).collect::<Vec<(&str, &str)>>()
}

fn parse_line(line: &str) -> Result<(&str, &str), Box<dyn std::error::Error>> {
    let trimmed_line = line.trim();
    let pieces = line.trim().split_at(trimmed_line.len()/2);
    Ok(pieces)
}

fn part1(rucksacks: &Vec<(&str, &str)>) -> u32 {
    rucksacks.iter().map(|(first, last)| {
        let first_set: HashSet<u8> = HashSet::from_iter(first.bytes());
        let last_set: HashSet<u8> = HashSet::from_iter(last.bytes());
        let common = first_set.intersection(&last_set);
        let mut common_iterator = common.into_iter();
        let common_byte = common_iterator.next().unwrap();
        if common_iterator.next() != None {panic!("oy!")}
        let score = match common_byte {
            b'a' ..= b'z' => {
                Into::<u32>::into(common_byte - b'a' + 1)
            }
            b'A'..= b'Z' => {
                Into::<u32>::into(common_byte - b'A' + 27)
            }
            _ => { panic!("oy2!") }
        };
        score
    }).sum::<u32>()
}

#[test]
fn check_part1() {
    let example = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw";
    assert_eq!(part1(&parse_input(example)), 157);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day03/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    Ok(())
}
