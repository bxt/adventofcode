use std::{collections::HashSet, iter::empty, ops::Add, vec};

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
    N,
    W,
    S,
    E,
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

trait Splitter {
    fn split(&self, previous_direction: Direction) -> Vec<Direction>;
}

impl Splitter for u8 {
    fn split(&self, previous_direction: Direction) -> Vec<Direction> {
        match (self, previous_direction) {
            (b'|', Direction::N | Direction::S) => vec![previous_direction],
            (b'|', Direction::W | Direction::E) => vec![Direction::N, Direction::S],
            (b'-', Direction::N | Direction::S) => vec![Direction::W, Direction::E],
            (b'-', Direction::W | Direction::E) => vec![previous_direction],
            (b'/', Direction::N) => vec![Direction::E],
            (b'/', Direction::W) => vec![Direction::S],
            (b'/', Direction::S) => vec![Direction::W],
            (b'/', Direction::E) => vec![Direction::N],
            (b'\\', Direction::N) => vec![Direction::W],
            (b'\\', Direction::W) => vec![Direction::N],
            (b'\\', Direction::S) => vec![Direction::E],
            (b'\\', Direction::E) => vec![Direction::S],
            (b'.', _) => vec![previous_direction],
            _ => panic!("Byte '{self}' is not a valid field entry!"),
        }
    }
}

fn advance_positions<'a>(
    beams: Vec<(Coord<isize>, Direction)>,
    field: &'a Vec<&'a [u8]>,
) -> impl Iterator<Item = (Coord<isize>, Direction)> + 'a {
    beams.into_iter().flat_map(move |(position, direction)| {
        position
            .on(field)
            .unwrap_or_else(|| panic!("Not on field? {position:?}"))
            .split(direction)
            .into_iter()
            .filter_map(move |direction| {
                let coord = position + Coord::from(&direction);
                coord.on(field).map(|_| (coord, direction))
            })
    })
}

fn find_energized_positions(start_beam: (Coord<isize>, Direction), field: &Vec<&[u8]>) -> usize {
    let mut seen = HashSet::from([start_beam]);

    let mut cycles = 0..;
    cycles.try_fold(vec![start_beam], |previous_beams, _| {
        let beams = advance_positions(previous_beams, &field)
            .filter(|beam| !seen.contains(beam))
            .collect::<Vec<_>>();

        for beam in &beams {
            seen.insert(beam.to_owned());
        }

        (beams.len() > 0).then_some(beams)
    });

    let mut energized_positions = HashSet::new();
    for (position, _) in seen {
        energized_positions.insert(position);
    }
    energized_positions.len()
}

fn main() -> () {
    let file = std::fs::read_to_string("day16/input.txt").unwrap();

    let field = file
        .lines()
        .map(|l| l.trim().as_bytes())
        .filter(|l| !l.is_empty())
        .collect::<Vec<_>>();

    let start_beam: (Coord<isize>, Direction) = (Coord(0, 0), Direction::E);
    println!("Part 1: {:?}", find_energized_positions(start_beam, &field));

    let potential_start_beams = empty()
        .chain((0..field[0].len()).map(|x| (Coord(0, x).into(), Direction::S)))
        .chain((0..field[0].len()).map(|x| (Coord(field.len() - 1, x).into(), Direction::N)))
        .chain((0..field.len()).map(|y| (Coord(y, 0).into(), Direction::E)))
        .chain((0..field.len()).map(|y| (Coord(y, field[0].len() - 1).into(), Direction::W)));
    let part2 = potential_start_beams
        .map(|start_beam| find_energized_positions(start_beam, &field))
        .max()
        .unwrap();
    println!("Part 2: {:?}", part2);
}
