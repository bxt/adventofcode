use std::collections::{HashMap, HashSet};
use std::iter::empty;
use std::str::FromStr;
use visualisation_utils::canvas::{Canvas, OffsetCanvas, PixelMap};
use visualisation_utils::encoder::LoopEncoder;
use visualisation_utils::font::Font;

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

    fn opposite(&self) -> Self {
        match self {
            Direction::N => Direction::S,
            Direction::W => Direction::E,
            Direction::S => Direction::N,
            Direction::E => Direction::W,
        }
    }
    fn is_horizontal(&self) -> bool {
        match self {
            Direction::N => false,
            Direction::W => true,
            Direction::S => false,
            Direction::E => true,
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
            (Mirror::Vertical, Direction::N | Direction::S) => vec![previous_direction],
            (Mirror::Vertical, Direction::W | Direction::E) => vec![Direction::N, Direction::S],
            (Mirror::Horizontal, Direction::N | Direction::S) => vec![Direction::W, Direction::E],
            (Mirror::Horizontal, Direction::W | Direction::E) => vec![previous_direction],
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
            "|" => Ok(Mirror::Vertical),
            "-" => Ok(Mirror::Horizontal),
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

fn draw_energized_positions(
    canvas: &mut impl Canvas<Position = (usize, usize)>,
    energized_positions: &HashMap<(usize, usize), HashSet<Direction>>,
) -> () {
    for (position, _) in energized_positions {
        let (x, y) = (position.0 * 3, position.1 * 3);
        for xo in 0..3 {
            for yo in 0..3 {
                canvas.set((x + xo, y + yo), 1);
            }
        }
    }
}

fn draw_mirrors(
    canvas: &mut impl Canvas<Position = (usize, usize)>,
    mirrors: &HashMap<(usize, usize), Mirror>,
) -> () {
    for (position, mirror) in mirrors {
        let (x, y) = (position.0 * 3, position.1 * 3);
        let offsets = match mirror {
            Mirror::Horizontal => [(0, 1), (1, 1), (2, 1)],
            Mirror::Vertical => [(1, 0), (1, 1), (1, 2)],
            Mirror::Rising => [(0, 2), (1, 1), (2, 0)],
            Mirror::Falling => [(0, 0), (1, 1), (2, 2)],
        };
        for (xo, yo) in offsets {
            canvas.set((x + xo, y + yo), 3);
        }
    }
}

fn draw_beams(
    canvas: &mut impl Canvas<Position = (usize, usize)>,
    beams: &Vec<((usize, usize), Direction)>,
) -> () {
    for (position, direction) in beams {
        let (x, y) = (position.0 * 3, position.1 * 3);
        let offsets = match direction {
            Direction::N => [(1, 0), (1, 1), (1, 2), (0, 0), (0, 1)],
            Direction::W => [(0, 1), (1, 1), (2, 1), (0, 0), (1, 0)],
            Direction::S => [(1, 0), (1, 1), (1, 2), (2, 1), (2, 2)],
            Direction::E => [(0, 1), (1, 1), (2, 1), (1, 2), (2, 2)],
        };
        for (xo, yo) in offsets {
            canvas.set((x + xo, y + yo), 5);
        }
    }
}

fn draw_enclosure(
    canvas: &mut impl Canvas<Position = (usize, usize)>,
    energized_positions_counts: &HashMap<((usize, usize), Direction), usize>,
    (field_width, field_height): (usize, usize),
    pad: usize,
) -> () {
    for x in 0..(field_width * 3 + 2) {
        canvas.set((x + pad - 1, pad - 1), 1);
        canvas.set((x + pad - 1, field_height * 3 + pad + 1), 2);
    }
    for y in 0..(field_height * 3 + 2) {
        canvas.set((pad - 1, y + pad - 1), 1);
        canvas.set((field_width * 3 + pad + 1, y + pad - 1), 2);
    }
    let part2 = energized_positions_counts.values().max().unwrap();
    for ((start, direction), count) in energized_positions_counts {
        let mut position = (pad + start.0 * 3, pad + start.1 * 3);
        let mut countdown = *count;
        (0..3).for_each(|_| {
            position = direction.opposite().walk(position).unwrap();
        });

        while countdown > 0 {
            if countdown & 1 != 0 {
                let offsets = if direction.is_horizontal() {
                    [(1, 0), (1, 1), (1, 2)]
                } else {
                    [(0, 1), (1, 1), (2, 1)]
                };
                let (x, y) = position;
                for (xo, yo) in offsets {
                    canvas.set((x + xo, y + yo), if count == part2 { 5 } else { 2 });
                }
            }
            position = direction.opposite().walk(position).unwrap();
            countdown >>= 1;
        }
    }
}

fn main() -> () {
    let start = std::time::Instant::now();

    let file = std::fs::read_to_string("day16/input.txt").unwrap();

    let font = Font::from_file(7, 9, 83, "../2021/visualisation_utils/font.pbm");
    let pad = 16;

    let field = file
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .collect::<Vec<_>>();

    let field_width = field[0].len();
    let field_height = field.len();
    let field_size = (field_width, field_height);

    let width = field_width * 3 + pad * 2;
    let height = field_height * 3 + pad * 3 + font.line_height();
    println!("width: {:?}, height: {:?}", width, height);

    let mut encoder = LoopEncoder::new("day16/output.gif", (width, height));

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

        let mut frame = 0;

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

            let record = *energized_positions_counts.values().max().unwrap();

            let is_new_record = energized_positions.len() == record;
            let is_end = beams.len() == 0;
            let is_first_row_done = energized_positions_counts.len() > field_height;
            let start_frames = if is_first_row_done { 0 } else { 3 };
            let end_frames = if is_first_row_done { 2 } else { 5 };
            let is_very_end =
                is_end && energized_positions_counts.len() == field_height * 2 + field_width * 2;
            let frame_count = if is_very_end {
                50
            } else if is_end {
                if is_new_record && is_first_row_done {
                    16
                } else {
                    end_frames
                }
            } else if frame < start_frames || is_new_record {
                1
            } else {
                0
            };
            for _ in 0..frame_count {
                let mut pixel_map = PixelMap::new((width, height));
                let mut padded_pixel_map = OffsetCanvas::new(&mut pixel_map, (pad, pad));
                draw_energized_positions(&mut padded_pixel_map, &energized_positions);
                draw_mirrors(&mut padded_pixel_map, &mirrors);
                draw_beams(&mut padded_pixel_map, &beams);
                draw_enclosure(&mut pixel_map, &energized_positions_counts, field_size, pad);

                let part1 = energized_positions_counts.get(&start_part_1).unwrap();
                let part2 = record;

                font.write_text(
                    &mut pixel_map,
                    format!("P1 {part1:4} P2 {part2:4}",).as_str(),
                    (pad, height - pad / 2 - font.line_height()),
                    if is_very_end { 5 } else { 2 },
                );

                encoder.write(pixel_map.to_vec());
            }

            frame += 1;
        }
    }

    let part1 = energized_positions_counts.get(&start_part_1).unwrap();
    println!("Part 1: {:?}", part1);

    let part2 = energized_positions_counts.values().max().unwrap();
    println!("Part 2: {:?}", part2);

    println!("Took: {:.2?}", start.elapsed());
}
