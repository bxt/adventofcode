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
    let mut current_number = None;
    let mut current_symbol = None;

    let mut windows = lines.windows(3);

    while let Some(&[prev_line, line, next_line]) = windows.next() {
        for (index, letter) in line.split("").enumerate() {
            match letter.parse::<u32>() {
                Ok(number) => {
                    match current_number {
                        None => current_number = Some(number),
                        Some(prev_number) => current_number = Some(prev_number * 10 + number),
                    }
                    let neighbors = vec![
                        current_symbol,
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
                    current_symbol = neighbors
                        .into_iter()
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

    Ok(())
}
