use std::cmp::{max, min};
use std::collections::HashSet;
use std::ops::{Add, Mul};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Coord<T>(T, T);

impl<T: Add<Output = T>> Add for Coord<T> {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Self(self.0 + other.0, self.1 + other.1)
    }
}

impl<T: Copy + Mul<T, Output = T>> Mul<T> for Coord<T> {
    type Output = Self;

    fn mul(self, rhs: T) -> Self::Output {
        Self(self.0 * rhs, self.1 * rhs)
    }
}

fn manhattan_distance(Coord(y1, x1): Coord<usize>, Coord(y2, x2): Coord<usize>) -> usize {
    max(y1, y2) - min(y1, y2) + max(x1, x2) - min(x1, x2)
}

struct SpaceTimeContinuum {
    galaxy_positions: Vec<Coord<usize>>,
    expanded_lines: HashSet<usize>,
    expanded_columns: HashSet<usize>,
}

impl From<String> for SpaceTimeContinuum {
    fn from(value: String) -> Self {
        let mut galaxy_positions = vec![];
        let cleaned_enumerated_lines = value
            .lines()
            .map(|l| l.trim().as_bytes())
            .filter(|l| !l.is_empty())
            .enumerate();
        for (line_index, bytes) in cleaned_enumerated_lines {
            for (index, byte) in bytes.iter().enumerate() {
                match byte {
                    b'#' => galaxy_positions.push(Coord(line_index, index)),
                    b'.' => {}
                    _ => panic!("Not sure what do do with {byte:?}"),
                }
            }
        }

        let max_line = galaxy_positions.iter().map(|g| g.0).max().unwrap();
        let max_column = galaxy_positions.iter().map(|g| g.1).max().unwrap();

        let mut expanded_lines = HashSet::from_iter(0..max_line);
        let mut expanded_columns = HashSet::from_iter(0..max_column);
        for Coord(line, column) in galaxy_positions.iter() {
            expanded_lines.remove(line);
            expanded_columns.remove(column);
        }

        Self {
            galaxy_positions,
            expanded_lines,
            expanded_columns,
        }
    }
}

impl SpaceTimeContinuum {
    fn expand(&self, times: usize) -> Vec<Coord<usize>> {
        self.galaxy_positions
            .iter()
            .map(|&coord| {
                let Coord(line, col) = coord;
                let line_expansion = self.expanded_lines.iter().filter(|&&l| l < line).count();
                let col_expansion = self.expanded_columns.iter().filter(|&&c| c < col).count();
                coord + Coord(line_expansion, col_expansion) * (times - 1)
            })
            .collect()
    }
}

fn total_distances(galaxy_positions: Vec<Coord<usize>>) -> usize {
    let mut sum = 0;
    for &g1 in galaxy_positions.iter() {
        for &g2 in galaxy_positions.iter() {
            sum += manhattan_distance(g1, g2);
        }
    }
    sum / 2
}

fn main() -> () {
    let file = std::fs::read_to_string("day11/input.txt").unwrap();
    let space_time_continuum = SpaceTimeContinuum::from(file);

    let part1 = total_distances(space_time_continuum.expand(2));
    println!("Part 1: {:?}", part1);

    let part2 = total_distances(space_time_continuum.expand(1000000));
    println!("Part 2: {:?}", part2);
}
