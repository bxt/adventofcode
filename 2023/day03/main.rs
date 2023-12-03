use std::collections::{HashMap, HashSet};
use std::iter::once;
use std::ops::Add;

fn try_into_symbol(letter: &str) -> Option<&str> {
    let is_symbol = letter != "." && letter.parse::<u32>().is_err();
    is_symbol.then_some(letter)
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Coord<T>(T, T);

impl<T: Add<Output = T>> Add for Coord<T> {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Self(self.0 + other.0, self.1 + other.1)
    }
}

impl Coord<i32> {
    fn eight_neighbors(self) -> Vec<Self> {
        let offsets = vec![
            Coord(-1, -1),
            Coord(0, -1),
            Coord(1, -1),
            Coord(-1, 0),
            Coord(1, 0),
            Coord(-1, 1),
            Coord(0, 1),
            Coord(1, 1),
        ];
        offsets.into_iter().map(|coord| self + coord).collect()
    }

    fn on<'a>(self, field: &'a Vec<&'a str>) -> Option<&str> {
        (self.0 >= 0 && self.1 >= 0)
            .then(|| {
                let index0 = usize::try_from(self.0).unwrap();
                let index1 = usize::try_from(self.1).unwrap();
                (index0 < field.len() && index1 < field[index0].len())
                    .then(|| &field[index0][index1..index1 + 1])
            })
            .flatten()
    }
}

fn enumerate_field<'a>(field: &'a Vec<&'a str>) -> impl Iterator<Item = (i32, i32)> + 'a {
    (0..field.len()).map(|index0| {
        let line = field[index0];
        let index0_i32 = i32::try_from(index0).unwrap();
        let index1_extend = i32::try_from(line.len()).unwrap();
        (index0_i32, index1_extend)
    })
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day03/input.txt")?;

    let lines = file
        .split("\n")
        .map(|line| line.trim())
        .collect::<Vec<&str>>();

    let mut numbers = vec![];
    let mut gears = HashMap::new();

    for (line_index, line_length) in enumerate_field(&lines) {
        let mut current_number = None;
        let mut current_symbol = None;
        let mut current_gears: HashSet<Coord<i32>> = HashSet::new();

        for index in 0..line_length + 1 {
            let coord = Coord(line_index, index);

            match coord.on(&lines).and_then(|l| l.parse::<u32>().ok()) {
                Some(digit) => {
                    match current_number {
                        None => current_number = Some(digit),
                        Some(prev_number) => current_number = Some(prev_number * 10 + digit),
                    }
                    let neighbors = coord.eight_neighbors();

                    let neighboring_gears = neighbors
                        .iter()
                        .filter_map(|&coord| (coord.on(&lines) == Some("*")).then_some(coord));
                    current_gears.extend(neighboring_gears);

                    current_symbol = neighbors
                        .into_iter()
                        .map(|coord| coord.on(&lines))
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
