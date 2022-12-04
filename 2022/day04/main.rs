use std::ops::Range;

type Couple = (Range<u32>, Range<u32>);

fn overlap(range1: &Range<u32>, range2: &Range<u32>) -> bool {
    range1.contains(&range2.start) && range1.contains(&(range2.end-1))
}

fn parse_input(input: &str) -> Vec<Couple> {
    input
        .trim()
        .split("\n")
        .flat_map(parse_line)
        .collect::<Vec<_>>()
}

fn parse_line(line: &str) -> Result<Couple, Box<dyn std::error::Error>> {
    let mut pieces = line.trim().split(",");
    let range1 = parse_range(pieces.next().ok_or("no first range")?)?;
    let range2 = parse_range(pieces.next().ok_or("no second range")?)?;
    Ok((range1, range2))
}

fn parse_range(line: &str) -> Result<Range<u32>, Box<dyn std::error::Error>> {
    let mut pieces = line.trim().split("-");
    let from: u32 = pieces.next().ok_or("no from")?.parse()?;
    let to: u32 = pieces.next().ok_or("no to")?.parse()?;
    Ok(from..to + 1)
}

fn part1(couples: Vec<Couple>) -> u32 {
    couples
        .iter()
        .map(|couple| {
            let (first, second) = couple;
            if overlap(first, second) || overlap(second, first) {
                1
            } else {
                0
            }
        })
        .sum::<u32>()
}

const EXAMPLE: &str = "\
    2-4,6-8\n\
    2-3,4-5\n\
    5-7,7-9\n\
    2-8,3-7\n\
    6-6,4-6\n\
    2-6,4-8\
    ";

#[test]
fn check_part1() {
    assert_eq!(part1(parse_input(EXAMPLE)), 2);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day04/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(parsed_input);
    println!("part 1: {}", part1);

    Ok(())
}
