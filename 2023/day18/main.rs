use std::{
    collections::{HashMap, HashSet},
    iter::empty,
    ops::Add,
    str::FromStr,
    vec,
};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Coord<T>(T, T);

impl<T: Add<Output = T>> Add for Coord<T> {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Self(self.0 + other.0, self.1 + other.1)
    }
}

impl Coord<isize> {
    fn on<'a, T>(self, field: &'a [&'a [T]]) -> Option<&T> {
        if self.0 < 0 || self.1 < 0 {
            return None;
        }
        let index0 = usize::try_from(self.0).unwrap();
        let index1 = usize::try_from(self.1).unwrap();
        (index0 < field.len() && index1 < field[index0].len()).then(|| &field[index0][index1])
    }
}

impl From<Coord<usize>> for Coord<isize> {
    fn from(Coord(y, x): Coord<usize>) -> Self {
        Coord(isize::try_from(y).unwrap(), isize::try_from(x).unwrap())
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Direction {
    U,
    L,
    D,
    R,
}

impl From<Direction> for Coord<isize> {
    fn from(value: Direction) -> Self {
        match value {
            Direction::U => Coord(-1, 0),
            Direction::L => Coord(0, -1),
            Direction::D => Coord(1, 0),
            Direction::R => Coord(0, 1),
        }
    }
}

impl FromStr for Direction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "U" => Ok(Direction::U),
            "L" => Ok(Direction::L),
            "D" => Ok(Direction::D),
            "R" => Ok(Direction::R),
            _ => Err("Not a valid direction: ".to_string() + s),
        }
    }
}

fn main() -> () {
    let file = std::fs::read_to_string("day18/input.txt").unwrap();

    let instructions = file
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .map(|line| {
            let (direction_str, rest) = line.split_once(" ").unwrap();
            let (distance_str, rest) = rest.split_once(" ").unwrap();
            let direction: Direction = direction_str.parse().unwrap();
            let distance: isize = distance_str.parse().unwrap();
            (direction, distance, rest)
        })
        .collect::<Vec<_>>();

    let mut trench = HashMap::new();
    let mut position = Coord(0, 0);

    for (direction, distance, color) in instructions {
        for _ in 0..distance {
            trench.insert(position, color);
            position = position + direction.into();
        }
    }

    let min_y = trench.keys().map(|c| c.0).min().unwrap();
    let max_y = trench.keys().map(|c| c.0).max().unwrap();
    let min_x = trench.keys().map(|c| c.1).min().unwrap();
    let max_x = trench.keys().map(|c| c.1).max().unwrap();

    dbg!(min_y);
    dbg!(min_y);
    dbg!(max_x);
    dbg!(max_x);

    let mut lagoon_size = 0;

    for y in min_y..=max_y {
        let mut is_in_lagoon = false;
        for x in min_x..=max_x {
            let position = Coord(y, x);
            if trench.contains_key(&position) {
                lagoon_size += 1;
                if trench.contains_key(&(position + Direction::D.into())) {
                    is_in_lagoon = !is_in_lagoon;
                }
            } else {
                if is_in_lagoon {
                    lagoon_size += 1;
                }
            }
        }
    }

    println!("Part 1: {:?}", lagoon_size);
}
