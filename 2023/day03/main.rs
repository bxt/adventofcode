use std::collections::{HashMap, HashSet};
use std::iter::once;

fn try_into_symbol(letter: &str) -> Option<&str> {
    let is_symbol = letter != "." && letter != "" && letter.parse::<u32>().is_err();
    is_symbol.then_some(letter)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day03/input.txt")?;

    let lines = once("")
        .chain(
            file.split("\n")
                .map(|line| line.trim())
                .collect::<Vec<&str>>(),
        )
        .chain(once(""))
        .collect::<Vec<&str>>();

    let mut numbers = vec![];
    let mut gears = HashMap::new();

    let mut current_number = None;
    let mut current_symbol = None;
    let mut current_gears = HashSet::new();

    let mut windows = lines.windows(3).enumerate();

    while let Some((line_index, &[prev_line, line, next_line])) = windows.next() {
        for (index, letter) in line.split("").enumerate() {
            match letter.parse::<u32>() {
                Ok(number) => {
                    match current_number {
                        None => current_number = Some(number),
                        Some(prev_number) => current_number = Some(prev_number * 10 + number),
                    }
                    let coords = vec![
                        (index != 0).then(|| ((line_index + 1) - 1, index - 1)),
                        (index != 0).then(|| ((line_index + 1), index - 1)),
                        (index != 0).then(|| ((line_index + 1) + 1, index - 1)),
                        Some(((line_index + 1) - 1, index)),
                        Some(((line_index + 1) + 1, index)),
                        Some(((line_index + 1) - 1, index + 1)),
                        Some(((line_index + 1), index + 1)),
                        Some(((line_index + 1) + 1, index + 1)),
                    ];
                    let neighbors = vec![
                        (index != 0)
                            .then(|| prev_line.split("").nth(index - 1))
                            .flatten(),
                        (index != 0)
                            .then(|| line.split("").nth(index - 1))
                            .flatten(),
                        (index != 0)
                            .then(|| next_line.split("").nth(index - 1))
                            .flatten(),
                        prev_line.split("").nth(index),
                        next_line.split("").nth(index),
                        prev_line.split("").nth(index + 1),
                        line.split("").nth(index + 1),
                        next_line.split("").nth(index + 1),
                    ];
                    neighbors
                        .iter()
                        .zip(coords)
                        .filter_map(|e| match e {
                            (Some("*"), coord) => coord,
                            _ => None,
                        })
                        .for_each(|coord| {
                            current_gears.insert(coord);
                        });
                    current_symbol = neighbors
                        .into_iter()
                        .chain(once(current_symbol))
                        .map(|n| n.and_then(try_into_symbol))
                        .find(|o| o.is_some())
                        .flatten();
                }
                Err(_) => match current_number {
                    None => {}
                    Some(number) => {
                        numbers.push((number, current_symbol));
                        current_number = None;
                        current_symbol = None;
                        for gear in current_gears {
                            let values = gears.entry(gear).or_insert_with(|| vec![]);
                            values.push(number);
                        }
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
        "part 3: {:?}",
        gears
            .iter()
            .filter_map(|(_, numbers)| (numbers.len() == 2).then(|| numbers[0] * numbers[1]))
            .sum::<u32>()
    );

    Ok(())
}
