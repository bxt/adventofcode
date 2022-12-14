use core::cmp::{max, min};
use std::collections::HashSet;

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

fn simulate_sand<const HAS_FLOOR: bool>(input: &Vec<Vec<(usize, usize)>>) -> usize {
    let abyss_after = *input
        .iter()
        .flat_map(|line| line.iter().map(|(_, y)| y))
        .max()
        .unwrap();

    let mut rocks = HashSet::new();
    for line in input {
        for window in line.windows(2) {
            match window {
                &[(from_x, from_y), (to_x, to_y)] => {
                    for x in min(from_x, to_x)..=max(from_x, to_x) {
                        for y in min(from_y, to_y)..=max(from_y, to_y) {
                            rocks.insert((x, y));
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    let mut sands = HashSet::new();
    let start_grain = (500, 0);

    'pouring: loop {
        let mut grain = start_grain;

        while let Some(new_grain) = {
            if !HAS_FLOOR && (grain.1 > abyss_after) {
                break 'pouring;
            }

            let (grain_x, grain_y) = grain;
            let candidates = [
                (grain_x, grain_y + 1),
                (grain_x - 1, grain_y + 1),
                (grain_x + 1, grain_y + 1),
            ];
            candidates.into_iter().find(|new_grain| {
                !rocks.contains(new_grain)
                    && !sands.contains(new_grain)
                    && (!HAS_FLOOR || new_grain.1 < abyss_after + 2)
            })
        } {
            grain = new_grain;
        }

        sands.insert(grain);

        if grain == start_grain {
            break 'pouring;
        }
    }

    sands.len()
}

fn part1(input: &Vec<Vec<(usize, usize)>>) -> usize {
    simulate_sand::<false>(input)
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

fn part2(input: &Vec<Vec<(usize, usize)>>) -> usize {
    simulate_sand::<true>(input)
}

#[test]
fn check_part2() {
    assert_eq!(
        part2(&parse_input(
            &std::fs::read_to_string("day14/example.txt").unwrap()
        )),
        93
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day14/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    let part2 = part2(&parsed_input);
    println!("part 2: {}", part2);

    Ok(())
}
