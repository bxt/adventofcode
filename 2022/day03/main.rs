use std::collections::HashSet;

fn parse_input(input: &str) -> Vec<&str> {
    input
        .trim()
        .split("\n")
        .map(str::trim)
        .collect::<Vec<&str>>()
}

fn get_priority(item: &u8) -> u32 {
    match item {
        b'a'..=b'z' => Into::<u32>::into(item - b'a' + 1),
        b'A'..=b'Z' => Into::<u32>::into(item - b'A' + 27),
        _ => {
            panic!("Unknown item: {}", item)
        }
    }
}

fn get_signleton_item<T>(collection: T) -> T::Item
where
    T: IntoIterator,
    <T as IntoIterator>::Item: PartialEq,
{
    let mut iterator = collection.into_iter();
    let item = iterator.next().unwrap();
    if iterator.next() != None {
        panic!("More than one item")
    }
    item
}

fn part1(rucksacks: &Vec<&str>) -> u32 {
    rucksacks
        .iter()
        .map(|rucksack| {
            let (first, last) = rucksack.trim().split_at(rucksack.len() / 2);
            let first_set = first.bytes().collect::<HashSet<u8>>();
            let last_set = last.bytes().collect::<HashSet<u8>>();
            let common = first_set.intersection(&last_set);
            get_priority(get_signleton_item(common))
        })
        .sum::<u32>()
}

const EXAMPLE: &str = "\
    vJrwpWtwJgWrhcsFMMfFFhFp\n\
    jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
    PmmdzqPrVvPwwTWBwg\n\
    wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
    ttgJtRGJQctTZtZT\n\
    CrZsJsPPZsGzwwsLwLmpwMDw\
    ";

#[test]
fn check_part1() {
    assert_eq!(part1(&parse_input(EXAMPLE)), 157);
}

fn part2(rucksacks: &Vec<&str>) -> u32 {
    rucksacks
        .chunks(3)
        .map(|chunk| {
            let sets = chunk
                .iter()
                .map(|rucksack| rucksack.bytes().collect::<HashSet<u8>>())
                .collect::<Vec<_>>();
            let sets_slice = sets.as_slice();
            match sets_slice {
                [set1, set2, set3] => {
                    let mut common = set1.iter().collect::<HashSet<&u8>>();
                    common.retain(|byte| set2.contains(byte) && set3.contains(byte));
                    get_priority(get_signleton_item(common))
                }
                _ => {
                    panic!("Chunk did not contain 3 items?")
                }
            }
        })
        .sum::<u32>()
}

#[test]
fn check_part2() {
    assert_eq!(part2(&parse_input(EXAMPLE)), 70);
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
