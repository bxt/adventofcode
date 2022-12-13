use std::cmp::Ordering;
use std::iter::Peekable;
use std::str::FromStr;
use std::vec;

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
    List(Vec<Self>),
}

#[derive(Debug)]
struct PacketParseError(String);

impl FromBytesIter for Packet {
    type FromBytesIterError = PacketParseError;

    fn from_bytes_iter(
        input: &mut Peekable<impl Iterator<Item = u8>>,
    ) -> Result<Self, PacketParseError> {
        match input.peek() {
            Some(b'[') => Vec::from_bytes_iter(input).map(Self::List),
            Some(b'0'..=b'9') => u8::from_bytes_iter(input).map(Self::Number),
            Some(other_char) => Err(PacketParseError(format!("unexpected char {other_char}"))),
            None => Err(PacketParseError("unexpected end".to_string())),
        }
    }
}

impl FromBytesIter for u8 {
    type FromBytesIterError = PacketParseError;

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

impl<T> FromBytesIter for Vec<T>
where
    T: FromBytesIter,
    PacketParseError: From<<T as FromBytesIter>::FromBytesIterError>,
{
    type FromBytesIterError = PacketParseError;

    fn from_bytes_iter(
        input: &mut Peekable<impl Iterator<Item = u8>>,
    ) -> Result<Self, Self::FromBytesIterError> {
        input
            .next_if_eq(&b'[')
            .ok_or(PacketParseError("expected opening bracket".to_string()))?;

        if input.next_if_eq(&b']').is_some() {
            return Ok(vec![]);
        }

        let mut children = vec![];

        children.push(T::from_bytes_iter(input)?);

        while input.next_if_eq(&b',').is_some() {
            children.push(T::from_bytes_iter(input)?);
        }

        (input.next() == Some(b']'))
            .then_some(children)
            .ok_or(PacketParseError("expected closing bracket".to_string()))
    }
}

impl FromStr for Packet {
    type Err = PacketParseError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        return Self::from_bytes_iter(&mut input.bytes().peekable());
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
            (a.parse().unwrap(), b.parse().unwrap())
        })
        .collect()
}

fn part1(input: &Vec<(Packet, Packet)>) -> usize {
    input
        .iter()
        .enumerate()
        .filter_map(|(index, (p1, p2))| (p1 <= p2).then_some(index + 1))
        .sum()
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

fn part2(input: &Vec<(Packet, Packet)>) -> usize {
    let dividers = [2, 6].map(|n| Packet::List(vec![Packet::List(vec![Packet::Number(n)])]));

    let mut packets = vec![];
    packets.extend(dividers.iter());
    for (a, b) in input {
        packets.push(a);
        packets.push(b);
    }
    packets.sort_unstable();

    packets
        .iter()
        .enumerate()
        .filter_map(|(index, p)| dividers.contains(p).then_some(index + 1))
        .product()
}

#[test]
fn check_part2() {
    assert_eq!(
        part2(&parse_input(
            &std::fs::read_to_string("day13/example.txt").unwrap()
        )),
        140
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day13/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    let part2 = part2(&parsed_input);
    println!("part 2: {}", part2);

    Ok(())
}
