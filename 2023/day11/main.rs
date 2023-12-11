use std::{collections::HashSet, ops::Add};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Coord<T>(T, T);

impl<T: Add<Output = T>> Add for Coord<T> {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Self(self.0 + other.0, self.1 + other.1)
    }
}

impl Coord<isize> {
    fn on<'a, T>(self, field: &'a Vec<Vec<T>>) -> Option<&T> {
        if self.0 < 0 || self.1 < 0 {
            return None;
        }
        let index0 = usize::try_from(self.0).unwrap();
        let index1 = usize::try_from(self.1).unwrap();
        (index0 < field.len() && index1 < field[index0].len()).then(|| &field[index0][index1])
    }
}

fn enumerate_field<'a, T>(field: &'a Vec<Vec<T>>) -> impl Iterator<Item = (isize, isize)> + 'a {
    (0..field.len()).map(|index0| {
        let line = &field[index0];
        let index0_isize = isize::try_from(index0).unwrap();
        let index1_extend = isize::try_from(line.len()).unwrap();
        (index0_isize, index1_extend)
    })
}

fn manhattan_distance(Coord(y1, x1): Coord<isize>, Coord(y2, x2): Coord<isize>) -> isize {
    (y2 - y1).abs() + (x2 - x1).abs()
}

fn main() -> () {
    let file = std::fs::read_to_string("day11/input.txt").unwrap();

    let field = file
        .lines()
        .map(|l| l.trim().as_bytes())
        .filter(|l| !l.is_empty())
        .map(|bytes| {
            bytes
                .iter()
                .map(|byte| match byte {
                    b'#' => true,
                    b'.' => false,
                    _ => panic!("Not sure what do do with {byte:?}"),
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let mut expanded_lines = HashSet::new();
    for (line_index, line_length) in enumerate_field(&field) {
        let no_galaxies = !(0..line_length)
            .map(|index| {
                let coord = Coord(line_index, index);
                coord.on(&field).unwrap()
            })
            .any(|&b| b);
        if no_galaxies {
            expanded_lines.insert(line_index);
        }
    }

    let mut expanded_columns = HashSet::new();
    let (_, line_length) = enumerate_field(&field).next().unwrap();
    for index in (0..line_length) {
        let no_galaxies = !enumerate_field(&field)
            .map(|(line_index, _)| {
                let coord = Coord(line_index, index);
                coord.on(&field).unwrap()
            })
            .any(|&b| b);
        if no_galaxies {
            expanded_columns.insert(index);
        }
    }

    let mut galaxy_positions = vec![];
    for (line_index, line_length) in enumerate_field(&field) {
        for index in 0..line_length {
            let coord = Coord(line_index, index);
            let &value = coord.on(&field).unwrap();
            if value {
                let expansion = Coord(
                    isize::try_from(expanded_lines.iter().filter(|&&l| l < line_index).count())
                        .unwrap(),
                    isize::try_from(expanded_columns.iter().filter(|&&c| c < index).count())
                        .unwrap(),
                );
                galaxy_positions.push(coord + expansion)
            }
        }
    }

    let mut total_distances = 0;
    for &g1 in &galaxy_positions {
        for &g2 in &galaxy_positions {
            total_distances += manhattan_distance(g1, g2)
        }
    }

    println!("Part 1: {:?}", total_distances / 2);

    // println!("Part 2: {:?}", 0);
}
