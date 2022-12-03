use std::collections::HashSet;
use std::iter::FromIterator;

fn parse_input(input: &str) -> Vec<&str> {
    input.trim()
    .split("\n")
    .map(|line| line.trim()).collect::<Vec<&str>>()
}

fn get_priority(item: &u8) -> u32 {
    match item {
        b'a' ..= b'z' => {
            Into::<u32>::into(item - b'a' + 1)
        }
        b'A'..= b'Z' => {
            Into::<u32>::into(item - b'A' + 27)
        }
        _ => { panic!("oy2!") }
    }
}

fn part1(rucksacks: &Vec<&str>) -> u32 {
    rucksacks.iter().map(|rucksack| {
        let (first, last) = rucksack.trim().split_at(rucksack.len()/2);
        let first_set: HashSet<u8> = HashSet::from_iter(first.bytes());
        let last_set: HashSet<u8> = HashSet::from_iter(last.bytes());
        let common = first_set.intersection(&last_set);
        let mut common_iterator = common.into_iter();
        let common_byte = common_iterator.next().unwrap();
        if common_iterator.next() != None {panic!("oy!")}
        get_priority(common_byte)
    }).sum::<u32>()
}

#[test]
fn check_part1() {
    let example = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw";
    assert_eq!(part1(&parse_input(example)), 157);
}

fn part2(rucksacks: &Vec<&str>) -> u32 {
    rucksacks.chunks(3).map(|chunk| {
        match chunk {
            [rucksack1, rucksack2, rucksack3] => {
                let set1: HashSet<u8> = HashSet::from_iter(rucksack1.bytes());
                let set2: HashSet<u8> = HashSet::from_iter(rucksack2.bytes());
                let set3: HashSet<u8> = HashSet::from_iter(rucksack3.bytes());

                let mut common = set1;
                common.retain(|byte| set2.contains(byte) && set3.contains(byte));
                let mut common_iterator = common.into_iter();
                let common_byte = common_iterator.next().unwrap();
                if common_iterator.next() != None {panic!("oy!")}
                get_priority(&common_byte)
            }
            _ => { panic!("oy3!") }
        }
    }).sum::<u32>()
}

#[test]
fn check_part2() {
    let example = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw";
    assert_eq!(part2(&parse_input(example)), 70);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day03/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);


    let part2 = part2(&parsed_input);
    println!("part 2: {}", part2);

    Ok(())
}
