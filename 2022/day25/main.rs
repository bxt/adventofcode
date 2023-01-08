use std::fmt::{Display, Formatter};
use std::str::FromStr;

use strum::IntoEnumIterator;
use strum_macros::{EnumIter, EnumString};

#[derive(Debug, Clone, Copy, PartialEq, EnumString, EnumIter, strum_macros::Display)]
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

impl From<SnafuDigit> for i64 {
    fn from(input: SnafuDigit) -> i64 {
        input as i64
    }
}

impl From<i64> for SnafuDigit {
    fn from(input: i64) -> Self {
        Self::iter().find(|&e| i64::from(e) == input).unwrap()
    }
}

struct SnafuNumber(Vec<SnafuDigit>);

impl From<&SnafuNumber> for u64 {
    fn from(number: &SnafuNumber) -> Self {
        let mut value = 0;
        for &digit in &number.0 {
            value *= 5;
            let digit_value = i64::from(digit);
            value += digit_value;
        }
        value.try_into().unwrap()
    }
}

impl From<u64> for SnafuNumber {
    fn from(input: u64) -> Self {
        let mut number = input;
        let mut digits = vec![];
        while number != 0 {
            let (d, m_u64) = (number / 5, number % 5);
            number = d;
            let m = i64::try_from(m_u64).unwrap();
            if m > 2 {
                number += 1;
                digits.push(SnafuDigit::from(m - 5));
            } else {
                digits.push(SnafuDigit::from(m));
            }
        }
        digits.reverse();
        Self(digits)
    }
}

impl FromStr for SnafuNumber {
    type Err = std::fmt::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(
            s.split("")
                .filter_map(|letter| letter.parse().ok())
                .collect(),
        ))
    }
}

impl Display for SnafuNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for digit in &self.0 {
            write!(f, "{}", digit)?;
        }
        Ok(())
    }
}

fn parse_input(input: &str) -> Vec<SnafuNumber> {
    input.trim().lines().map(|s| s.parse().unwrap()).collect()
}

fn part1(input: &Vec<SnafuNumber>) -> String {
    let sum: u64 = input.into_iter().map(u64::from).sum();
    SnafuNumber::from(sum).to_string()
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
        assert_eq!(u64::from(&snafu_digit_string.parse().unwrap()), number);
        assert_eq!(SnafuNumber::from(number).to_string(), snafu_digit_string);
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day25/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    Ok(())
}
