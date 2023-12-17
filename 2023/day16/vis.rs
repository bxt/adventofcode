use std::{
    collections::{HashMap, HashSet},
    iter::empty,
    str::FromStr,
};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Direction {
    N,
    W,
    S,
    E,
}

impl Direction {
    fn walk(&self, (x, y): (usize, usize)) -> Option<(usize, usize)> {
        match self {
            Direction::N => (y > 0).then(|| (x, y - 1)),
            Direction::W => (x > 0).then(|| (x - 1, y)),
            Direction::S => Some((x, y + 1)),
            Direction::E => Some((x + 1, y)),
        }
    }
}

enum Mirror {
    Horizontal,
    Vertical,
    Rising,
    Falling,
}

impl Mirror {
    fn split(&self, previous_direction: Direction) -> Vec<Direction> {
        match (self, previous_direction) {
            (Mirror::Horizontal, Direction::N | Direction::S) => vec![previous_direction],
            (Mirror::Horizontal, Direction::W | Direction::E) => vec![Direction::N, Direction::S],
            (Mirror::Vertical, Direction::N | Direction::S) => vec![Direction::W, Direction::E],
            (Mirror::Vertical, Direction::W | Direction::E) => vec![previous_direction],
            (Mirror::Rising, Direction::N) => vec![Direction::E],
            (Mirror::Rising, Direction::W) => vec![Direction::S],
            (Mirror::Rising, Direction::S) => vec![Direction::W],
            (Mirror::Rising, Direction::E) => vec![Direction::N],
            (Mirror::Falling, Direction::N) => vec![Direction::W],
            (Mirror::Falling, Direction::W) => vec![Direction::N],
            (Mirror::Falling, Direction::S) => vec![Direction::E],
            (Mirror::Falling, Direction::E) => vec![Direction::S],
        }
    }
}

impl FromStr for Mirror {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "|" => Ok(Mirror::Horizontal),
            "-" => Ok(Mirror::Vertical),
            "/" => Ok(Mirror::Rising),
            "\\" => Ok(Mirror::Falling),
            string => Err(format!("The '{string}' is not a valid mirror!")),
        }
    }
}

fn advance_positions<'a>(
    beams: Vec<((usize, usize), Direction)>,
    mirrors: &'a HashMap<(usize, usize), Mirror>,
    (field_width, field_height): (usize, usize),
) -> impl Iterator<Item = ((usize, usize), Direction)> + 'a {
    beams.into_iter().flat_map(move |(position, direction)| {
        mirrors
            .get(&position)
            .map(|mirror| mirror.split(direction))
            .unwrap_or_else(|| vec![direction])
            .into_iter()
            .filter_map(move |direction| {
                direction
                    .walk(position)
                    .filter(|&(x, y)| x < field_width && y < field_height)
                    .map(|coord| (coord, direction))
            })
    })
}

fn main() -> () {
    let file = std::fs::read_to_string("day16/input.txt").unwrap();

    let field = file
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .collect::<Vec<_>>();

    let field_width = field[0].len();
    let field_height = field.len();
    let field_size = (field_width, field_height);

    let mut mirrors = HashMap::new();
    for y in 0..field_height {
        for x in 0..field_width {
            if let Ok(mirror) = (&(field[y])[x..=x]).parse::<Mirror>() {
                mirrors.insert((x, y), mirror);
            };
        }
    }

    let start_part_1 = ((0, 0), Direction::E);
    let potential_starts = empty()
        .chain((0..field_height).map(|y| ((0, y), Direction::E)))
        .chain((0..field_width).map(|x| ((x, 0), Direction::S)))
        .chain((0..field_width).map(|x| ((x, field_height - 1), Direction::N)))
        .chain((0..field_height).map(|y| ((field_width - 1, y), Direction::W)));

    let mut energized_positions_counts = HashMap::new();

    for start in potential_starts {
        let mut energized_positions = HashMap::new();
        let mut beams = vec![start];

        while beams.len() != 0 {
            for (position, direction) in &beams {
                energized_positions
                    .entry(*position)
                    .or_insert_with(|| HashSet::new())
                    .insert(*direction);
            }

            beams = advance_positions(beams, &mirrors, field_size)
                .filter(|(position, direction)| {
                    !energized_positions
                        .get(position)
                        .is_some_and(|directions| directions.contains(direction))
                })
                .collect::<Vec<_>>();

            energized_positions_counts.insert(start, energized_positions.len());
        }
    }

    let part1 = energized_positions_counts.get(&start_part_1).unwrap();
    println!("Part 1: {:?}", part1);

    let part2 = energized_positions_counts.values().max().unwrap();
    println!("Part 2: {:?}", part2);
}
