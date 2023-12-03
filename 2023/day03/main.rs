use std::collections::{HashMap, HashSet};
use std::iter::once;

type Coord = (i32, i32);

fn try_into_symbol(letter: &str) -> Option<&str> {
    let is_symbol = letter != "." && letter.parse::<u32>().is_err();
    is_symbol.then_some(letter)
}

fn at<'a>(lines: &'a Vec<&'a str>, (line_index, index): Coord) -> Option<&str> {
    (line_index >= 0 && index >= 0)
        .then(|| {
            let line_index_usize = usize::try_from(line_index).unwrap();
            let index_usize = usize::try_from(index).unwrap();
            (line_index_usize < lines.len() && index_usize < lines[line_index_usize].len())
                .then(|| &lines[line_index_usize][index_usize..index_usize + 1])
        })
        .flatten()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day03/input.txt")?;

    let lines = file
        .split("\n")
        .map(|line| line.trim())
        .collect::<Vec<&str>>();

    let mut numbers = vec![];
    let mut gears = HashMap::new();

    for line_index_usize in 0..lines.len() {
        let line = lines[line_index_usize];
        let line_index = i32::try_from(line_index_usize).unwrap();

        let mut current_number = None;
        let mut current_symbol = None;
        let mut current_gears: HashSet<Coord> = HashSet::new();

        for index_usize in 0..(line.len() + 1) {
            let index = i32::try_from(index_usize).unwrap();
            let coord = (line_index, index);

            match at(&lines, coord).and_then(|letter| letter.parse::<u32>().ok()) {
                Some(digit) => {
                    match current_number {
                        None => current_number = Some(digit),
                        Some(prev_number) => current_number = Some(prev_number * 10 + digit),
                    }
                    let neighbors = vec![
                        (line_index - 1, index - 1),
                        (line_index, index - 1),
                        (line_index + 1, index - 1),
                        (line_index - 1, index),
                        (line_index + 1, index),
                        (line_index - 1, index + 1),
                        (line_index, index + 1),
                        (line_index + 1, index + 1),
                    ];

                    let neighboring_gears = neighbors
                        .iter()
                        .filter_map(|&coord| (at(&lines, coord) == Some("*")).then_some(coord));
                    current_gears.extend(neighboring_gears);

                    current_symbol = neighbors
                        .into_iter()
                        .map(|coord| at(&lines, coord))
                        .chain(once(current_symbol))
                        .map(|l| l.and_then(try_into_symbol))
                        .find(|option| option.is_some())
                        .flatten();
                }
                None => match current_number {
                    None => {}
                    Some(number) => {
                        numbers.push((number, current_symbol));
                        for gear in current_gears {
                            let values = gears.entry(gear).or_insert_with(|| vec![]);
                            values.push(number);
                        }

                        current_number = None;
                        current_symbol = None;
                        current_gears = HashSet::new();
                    }
                },
            }
        }
    }

    println!(
        "part 1: {:?}",
        numbers
            .into_iter()
            .filter_map(|(number, symbol)| symbol.map(|_| number))
            .sum::<u32>()
    );

    println!(
        "part 2: {:?}",
        gears
            .iter()
            .filter_map(|(_, numbers)| (numbers.len() == 2).then(|| numbers[0] * numbers[1]))
            .sum::<u32>()
    );

    Ok(())
}
