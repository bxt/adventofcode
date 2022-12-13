use std::cmp::Ordering;
use std::vec;
use std::{
    collections::{HashMap, VecDeque},
    iter::Peekable,
    str::FromStr,
};

trait FromBytesIter {
    type FromBytesIterError;
    fn from_bytes_iter(
        input: &mut Peekable<impl Iterator<Item = u8>>,
    ) -> Result<Self, Self::FromBytesIterError>
    where
        Self: Sized;
}

#[derive(Debug, PartialEq, Eq)]
enum Packet {
    Number(u8),
    List(Vec<Packet>),
}

#[derive(Debug)]
struct PacketParseError(String);

impl FromBytesIter for Packet {
    type FromBytesIterError = PacketParseError;

    fn from_bytes_iter(
        input: &mut Peekable<impl Iterator<Item = u8>>,
    ) -> Result<Self, PacketParseError> {
        match input.peek() {
            Some(b'[') => {
                input.next();

                if input.peek() == Some(&b']') {
                    input.next();
                    return Ok(Packet::List(vec![]));
                }

                let mut children = vec![];

                let first = Packet::from_bytes_iter(input)?;
                children.push(first);

                while input.peek() == Some(&b',') {
                    input.next();
                    let additional = Packet::from_bytes_iter(input)?;
                    children.push(additional);
                }

                if input.next() == Some(b']') {
                    Ok(Packet::List(children))
                } else {
                    Err(PacketParseError("expected closing bracket".to_string()))
                }
            }
            Some(b'0'..=b'9') => u8::from_bytes_iter(input)
                .map(Packet::Number)
                .map_err(|_| PacketParseError("number not parsable?".to_string())),
            Some(other_char) => Err(PacketParseError(
                format!("unexpected char {other_char}").to_string(),
            )),
            None => Err(PacketParseError("unexpected end".to_string())),
        }
    }
}

impl FromBytesIter for u8 {
    type FromBytesIterError = std::fmt::Error;

    fn from_bytes_iter(
        input: &mut Peekable<impl Iterator<Item = u8>>,
    ) -> Result<Self, Self::FromBytesIterError> {
        let mut number = 0;
        while let Some(byte @ b'0'..=b'9') = input.peek() {
            number *= 10;
            number += byte - b'0';
            input.next();
        }
        Ok(number)
    }
}

impl FromStr for Packet {
    type Err = PacketParseError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        return Packet::from_bytes_iter(&mut input.bytes().peekable());
    }
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Number(a), Self::Number(b)) => a.cmp(b),
            (Self::List(a), Self::List(b)) => a.cmp(b),
            (Self::List(a), Self::Number(b)) => a.cmp(&vec![Self::Number(*b)]),
            (Self::Number(a), Self::List(b)) => vec![Self::Number(*a)].cmp(b),
        }
    }
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn parse_input(input: &str) -> Vec<(Packet, Packet)> {
    input
        .split("\n\n")
        .map(|block| {
            let (a, b) = block.split_once("\n").unwrap();
            // println!("a: {a}");
            let pa = a.parse().unwrap();
            // println!("b: {b}");
            let pb = b.parse().unwrap();
            (pa, pb)
        })
        .collect()
}

fn part1(input: &Vec<(Packet, Packet)>) -> usize {
    // println!("{input:?}");

    let good_positions = input
        .iter()
        .enumerate()
        .filter_map(|(index, (p1, p2))| (p1 <= p2).then_some(index + 1))
        .collect::<Vec<_>>();
    println!("{good_positions:?}");

    good_positions.iter().sum()
}

#[test]
fn check_part1() {
    assert_eq!(
        part1(&parse_input(
            &std::fs::read_to_string("day13/example.txt").unwrap()
        )),
        13
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day13/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    // let part2 = part2(&parsed_input);
    // println!("part 2: {}", part2);

    Ok(())
}
