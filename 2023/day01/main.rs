fn parse_line(line: &str, include_names: bool) -> Vec<u32> {
    let trimmed = line.trim();

    let mut numbers = vec![];
    for n in 0..trimmed.len() {
        if let Ok(number) = trimmed[n..n + 1].parse::<u32>() {
            numbers.push(number)
        } else if include_names {
            let next_letters = &trimmed[n..std::cmp::min(n + 5, trimmed.len())];
            if next_letters.starts_with("one") {
                numbers.push(1);
            } else if next_letters.starts_with("two") {
                numbers.push(2);
            } else if next_letters.starts_with("three") {
                numbers.push(3);
            } else if next_letters.starts_with("four") {
                numbers.push(4);
            } else if next_letters.starts_with("five") {
                numbers.push(5);
            } else if next_letters.starts_with("six") {
                numbers.push(6);
            } else if next_letters.starts_with("seven") {
                numbers.push(7);
            } else if next_letters.starts_with("eight") {
                numbers.push(8);
            } else if next_letters.starts_with("nine") {
                numbers.push(9);
            }
        }
    }
    numbers
}

#[test]
fn check_parse_line() {
    assert_eq!(
        parse_line("123456789", false),
        vec![1, 2, 3, 4, 5, 6, 7, 8, 9]
    );
    assert_eq!(parse_line("one two 3", false), vec![3]);
    assert_eq!(parse_line("one two 3", true), vec![1, 2, 3]);
    assert_eq!(
        parse_line("one two three four five six seven eight nine", true),
        vec![1, 2, 3, 4, 5, 6, 7, 8, 9]
    );
    assert_eq!(parse_line("eightwothree", true), vec![8, 2, 3]);
}

const EXAMPLE: &str = "\
    two1nine\n\
    eightwothree\n\
    abcone2threexyz\n\
    xtwone3four\n\
    4nineeightseven2\n\
    zoneight234\n\
    7pqrstsixteen\n\
    ";

#[test]
fn check_calibration_value_sum() {
    assert_eq!(calibration_value_sum(EXAMPLE, true), 281);
}

fn calibration_value_sum(input: &str, include_names: bool) -> u32 {
    input
        .split("\n")
        .filter(|line| !line.is_empty())
        .map(|line| parse_line(&line, include_names))
        .map(|numbers| numbers[0] * 10 + numbers[numbers.len() - 1])
        .sum()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day01/input.txt")?;

    println!("part 1: {}", calibration_value_sum(&file, false));

    println!("part 2: {}", calibration_value_sum(&file, true));

    Ok(())
}
