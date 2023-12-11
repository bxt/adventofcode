use std::{collections::HashSet, ops::Add, vec};

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

fn enumerate_field<'a, T>(field: &'a [&'a [T]]) -> impl Iterator<Item = (isize, isize)> + 'a {
    (0..field.len()).map(|index0| {
        let line = field[index0];
        let index0_isize = isize::try_from(index0).unwrap();
        let index1_extend = isize::try_from(line.len()).unwrap();
        (index0_isize, index1_extend)
    })
}

fn find_field_index<'a, T, P: Fn(&T) -> bool>(
    field: &'a [&'a [T]],
    predicate: P,
) -> Option<Coord<isize>> {
    for (line_index, line_length) in enumerate_field(&field) {
        for index in 0..line_length {
            let coord = Coord(line_index, index);
            let value = coord.on(&field).unwrap();
            if predicate(value) {
                return Some(coord);
            }
        }
    }

    None
}

#[derive(Debug, PartialEq)]
enum Direction {
    N,
    W,
    S,
    E,
}

impl Direction {
    fn opposite(&self) -> Self {
        match self {
            Direction::N => Direction::S,
            Direction::W => Direction::E,
            Direction::S => Direction::N,
            Direction::E => Direction::W,
        }
    }
}

impl From<&Direction> for Coord<isize> {
    fn from(value: &Direction) -> Self {
        match value {
            Direction::N => Coord(-1, 0),
            Direction::W => Coord(0, -1),
            Direction::S => Coord(1, 0),
            Direction::E => Coord(0, 1),
        }
    }
}

trait Connector {
    fn directions(&self) -> Vec<Direction>;
}

impl Connector for u8 {
    fn directions(&self) -> Vec<Direction> {
        match self {
            b'|' => vec![Direction::N, Direction::S],
            b'-' => vec![Direction::W, Direction::E],
            b'L' => vec![Direction::N, Direction::E],
            b'J' => vec![Direction::N, Direction::W],
            b'7' => vec![Direction::W, Direction::S],
            b'F' => vec![Direction::S, Direction::E],
            b'.' => vec![],
            b'S' => vec![Direction::N, Direction::W, Direction::S, Direction::E],
            _ => panic!("Byte '{self}' is not a valid field entry!"),
        }
    }
}

impl Connector for Option<&u8> {
    fn directions(&self) -> Vec<Direction> {
        self.map_or(vec![], |b| b.directions())
    }
}

fn advance_positions(
    positions: Vec<Coord<isize>>,
    field: &Vec<&[u8]>,
    on_loop_positions: &mut HashSet<Coord<isize>>,
) -> Vec<Coord<isize>> {
    positions
        .into_iter()
        .flat_map(|position| {
            position
                .on(field)
                .directions()
                .into_iter()
                .filter_map(|direction| {
                    let coord = position + Coord::from(&direction);
                    if on_loop_positions.contains(&coord) {
                        return None;
                    }
                    if !coord.on(field).directions().contains(&direction.opposite()) {
                        return None;
                    }
                    on_loop_positions.insert(coord);
                    Some(coord)
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
}

fn find_enclosed_positions(
    field: Vec<&[u8]>,
    on_loop_positions: HashSet<Coord<isize>>,
) -> Vec<Coord<isize>> {
    let mut enclosed_positions = vec![];

    for (line_index, line_length) in enumerate_field(&field) {
        let mut is_inside_loop = false;

        for index in 0..line_length {
            let position = Coord(line_index, index);

            if !on_loop_positions.contains(&position) {
                if is_inside_loop {
                    enclosed_positions.push(position);
                }
                continue;
            }

            let value = *position.on(&field).unwrap();
            if value == b'S' {
                let coord = position + Coord::from(&Direction::N);
                if coord.on(&field).directions().contains(&Direction::S) {
                    is_inside_loop = !is_inside_loop;
                }
            } else if value.directions().contains(&Direction::N) {
                is_inside_loop = !is_inside_loop;
            }
        }
    }

    enclosed_positions
}

fn main() -> () {
    let file = std::fs::read_to_string("day10/input.txt").unwrap();

    let field = file
        .lines()
        .map(|l| l.trim().as_bytes())
        .filter(|l| !l.is_empty())
        .collect::<Vec<_>>();

    let start_position = find_field_index(&field, |&b| b == b'S').expect("No start?");
    let mut on_loop_positions = HashSet::from([start_position]);

    let mut distances = 0..;
    distances.try_fold(vec![start_position], |previous_positions, _| {
        let positions = advance_positions(previous_positions, &field, &mut on_loop_positions);
        (positions.len() > 0).then_some(positions)
    });

    let furthest_distance = distances.next().unwrap() - 1;

    println!("Part 1: {:?}", furthest_distance);

    let enclosed_count = find_enclosed_positions(field, on_loop_positions).len();

    println!("Part 2: {:?}", enclosed_count);
}
