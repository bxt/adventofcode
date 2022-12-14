use core::cmp::{max, min};
use std::collections::HashSet;
use std::iter::Peekable;
use std::str::FromStr;
use std::vec;

fn parse_input(input: &str) -> Vec<Vec<(usize, usize)>> {
    input
        .lines()
        .map(|line| {
            line.split(" -> ")
                .map(|point| {
                    let (a, b) = point.split_once(",").unwrap();
                    (a.parse().unwrap(), b.parse().unwrap())
                })
                .collect()
        })
        .collect()
}

fn part1(input: &Vec<Vec<(usize, usize)>>) -> usize {
    println!("{input:?}");

    let abyss_after = *input
        .iter()
        .flat_map(|line| line.iter().map(|(_, y)| y))
        .max()
        .unwrap();
    println!("abyss_after: {abyss_after:?}");

    let mut rocks = HashSet::new();
    for line in input {
        for window in line.windows(2) {
            match window {
                &[(from_x, from_y), (to_x, to_y)] => {
                    for x in (min(from_x, to_x))..=(max(from_x, to_x)) {
                        for y in (min(from_y, to_y))..=(max(from_y, to_y)) {
                            rocks.insert((x, y));
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
    }
    println!("rocks: {rocks:?}");

    let mut sands = HashSet::new();

    'pouring: loop {
        let mut grain = (500, 0);

        'falling: loop {
            if grain.1 > abyss_after {
                break 'pouring;
            }

            let (start_x, start_y) = grain;
            let candidates = [
                (start_x, start_y + 1),
                (start_x - 1, start_y + 1),
                (start_x + 1, start_y + 1),
            ];
            let maybe_new_grain = candidates
                .into_iter()
                .find(|new_grain| !rocks.contains(new_grain) && !sands.contains(new_grain));

            match maybe_new_grain {
                Some(new_grain) => {
                    grain = new_grain;
                }
                None => {
                    sands.insert(grain);
                    break 'falling;
                }
            }
        }
    }

    sands.len()
}

#[test]
fn check_part1() {
    assert_eq!(
        part1(&parse_input(
            &std::fs::read_to_string("day14/example.txt").unwrap()
        )),
        24
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day14/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    // let part2 = part2(&parsed_input);
    // println!("part 2: {}", part2);

    Ok(())
}
