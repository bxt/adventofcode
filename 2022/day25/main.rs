use strum::IntoEnumIterator;
use strum_macros::{Display, EnumIter, EnumString};

#[derive(Debug, Clone, Copy, PartialEq, EnumString, EnumIter, Display)]
enum SnafuDigit {
    #[strum(serialize = "=")]
    DoubleMinus = -2,
    #[strum(serialize = "-")]
    Minus,
    #[strum(serialize = "0")]
    Zero,
    #[strum(serialize = "1")]
    One,
    #[strum(serialize = "2")]
    Two,
}

fn snafu_digits_from_str(input: &str) -> Vec<SnafuDigit> {
    input
        .split("")
        .filter_map(|letter| letter.parse().ok())
        .collect()
}

fn parse_input(input: &str) -> Vec<Vec<SnafuDigit>> {
    input.trim().lines().map(snafu_digits_from_str).collect()
}

fn i64_from_snafu_digit(input: SnafuDigit) -> i64 {
    input as i64
}

fn snafu_digit_from_i64(input: i64) -> SnafuDigit {
    SnafuDigit::iter().find(|&e| e as i64 == input).unwrap()
}

fn u64_from_snafu_digits(input: &Vec<SnafuDigit>) -> u64 {
    let mut value = 0;
    for &digit in input {
        value *= 5;
        value += i64_from_snafu_digit(digit);
    }
    value.try_into().unwrap()
}

fn snafu_digits_from_u64(input: u64) -> Vec<SnafuDigit> {
    let mut number = input;
    let mut digits = vec![];
    while number != 0 {
        let (d, m_u64) = (number / 5, number % 5);
        number = d;
        let m = i64::try_from(m_u64).unwrap();
        if m > 2 {
            number += 1;
            digits.push(snafu_digit_from_i64(m - 5));
        } else {
            digits.push(snafu_digit_from_i64(m));
        }
    }
    digits.reverse();
    digits
}

fn string_from_snafu_digits(input: Vec<SnafuDigit>) -> String {
    input
        .iter()
        .map(|d| d.to_string())
        .collect::<Vec<String>>()
        .join("")
}

fn part1(input: &Vec<Vec<SnafuDigit>>) -> String {
    let sum = input.iter().map(u64_from_snafu_digits).sum();
    string_from_snafu_digits(snafu_digits_from_u64(sum))
}

#[test]
fn check_part1() {
    let examples = vec![
        (1, "1"),
        (2, "2"),
        (3, "1="),
        (4, "1-"),
        (5, "10"),
        (6, "11"),
        (7, "12"),
        (8, "2="),
        (9, "2-"),
        (10, "20"),
        (15, "1=0"),
        (20, "1-0"),
        (2022, "1=11-2"),
        (12345, "1-0---0"),
        (314159265, "1121-1110-1=0"),
        (1747, "1=-0-2"),
        (906, "12111"),
        (198, "2=0="),
        (11, "21"),
        (201, "2=01"),
        (31, "111"),
        (1257, "20012"),
        (32, "112"),
        (353, "1=-1="),
        (107, "1-12"),
        (37, "122"),
    ];
    for (number, snafu_digit_string) in examples {
        assert_eq!(
            u64_from_snafu_digits(&snafu_digits_from_str(snafu_digit_string)),
            number
        );
        assert_eq!(
            string_from_snafu_digits(snafu_digits_from_u64(number)),
            snafu_digit_string
        );
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day25/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    Ok(())
}
