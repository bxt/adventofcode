use std::iter::once;
use std::{collections::HashSet, ops::Add, time::Instant, vec};
use visualisation_utils::canvas::{Canvas, MappedCanvas, OffsetCanvas, PixelMap};
use visualisation_utils::encoder::LoopEncoder;
use visualisation_utils::font::Font;

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

fn infer_value(field: &Vec<&[u8]>, position: Coord<isize>) -> Option<u8> {
    let connected_directions = b'S'
        .directions()
        .into_iter()
        .filter(|direction| {
            let coord = position + Coord::from(direction);
            coord.on(field).directions().contains(&direction.opposite())
        })
        .collect::<Vec<_>>();
    [b'|', b'-', b'L', b'J', b'7', b'F']
        .into_iter()
        .find(|b| b.directions() == connected_directions)
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
    field: &Vec<&[u8]>,
    on_loop_positions: &HashSet<Coord<isize>>,
) -> Vec<Coord<isize>> {
    let mut enclosed_positions = vec![];
    let mut is_inside_loop;
    let mut got_onto_loop_by;

    for (line_index, line_length) in enumerate_field(&field) {
        is_inside_loop = false;
        got_onto_loop_by = None;

        for index in 0..line_length {
            let position = Coord(line_index, index);

            if !on_loop_positions.contains(&position) {
                if is_inside_loop {
                    enclosed_positions.push(position);
                }
                continue;
            }

            let mut value = *position.on(&field).unwrap();
            if value == b'S' {
                value = infer_value(&field, position).expect("No match for start point found");
            }

            match (got_onto_loop_by, value) {
                (None, b'|') => {
                    is_inside_loop = !is_inside_loop;
                }
                (Some(_), b'|') => {
                    panic!("Encountered | on the loop at {:?}!", position);
                }

                (Some(_), b'-') => {}
                (None, b'L' | b'F') => {
                    got_onto_loop_by = Some(value);
                }
                (Some(b'L'), b'J') | (Some(b'F'), b'7') => {
                    got_onto_loop_by = None;
                }
                (Some(b'L'), b'7') | (Some(b'F'), b'J') => {
                    got_onto_loop_by = None;
                    is_inside_loop = !is_inside_loop;
                }
                (on, off) => {
                    panic!(
                        "Came onto loop with {:?} and went off with {:?} at {:?}!",
                        on, off, position
                    );
                }
            }
        }
    }

    enclosed_positions
}

fn draw_field(
    padded_pixel_map: &mut impl Canvas<Position = (usize, usize)>,
    field: &[&[u8]],
    on_loop_positions: &HashSet<Coord<isize>>,
    on_loop_color: u8,
) -> () {
    let mut canvas: MappedCanvas<isize, _> = MappedCanvas::new(padded_pixel_map);

    for (line_index, line_length) in enumerate_field(&field) {
        for index in 0..line_length {
            let position = Coord(line_index, index);
            let value = *position.on(&field).unwrap();

            let color = if on_loop_positions.contains(&position) {
                on_loop_color
            } else {
                1
            };

            let center = Coord(line_index * 3 + 1, index * 3 + 1);
            let pipes = value
                .directions()
                .into_iter()
                .map(|direction| center + Coord::from(&direction));

            for Coord(y, x) in once(center).chain(pipes) {
                canvas.set((x, y), color);
            }
        }
    }
}

fn draw_enclosed(
    padded_pixel_map: &mut impl Canvas<Position = (usize, usize)>,
    enclosed_positions: &[Coord<isize>],
    enclosed_color: u8,
) -> () {
    let mut canvas: MappedCanvas<isize, _> = MappedCanvas::new(padded_pixel_map);

    for Coord(line_index, index) in enclosed_positions {
        let (x, y) = (index * 3 + 1, line_index * 3 + 1);
        let offsets = [1, 0, -1];
        for xo in offsets {
            for yo in offsets {
                canvas.set((x + xo, y + yo), enclosed_color);
            }
        }
    }
}

fn main() -> () {
    let start = Instant::now();

    let file = std::fs::read_to_string("day10/input.txt").unwrap();
    let font = Font::from_file(7, 9, 83, "../2021/visualisation_utils/font.pbm");
    let pad = 8;
    let frame_every_distance = 8;
    let frame_every_enclosed = 8;

    let field = file
        .lines()
        .map(|l| l.trim().as_bytes())
        .filter(|l| !l.is_empty())
        .collect::<Vec<_>>();

    let width = field[0].len() * 3 + pad * 2;
    let height = field.len() * 3 + pad * 3 + font.line_height();
    println!("width: {:?}, height: {:?}", width, height);

    let mut encoder = LoopEncoder::new("day10/output.gif", (width, height));

    let start_position = find_field_index(&field, |&b| b == b'S').expect("No start?");
    let mut on_loop_positions = HashSet::from([start_position]);

    let mut distances = 0..;
    distances.try_fold(vec![start_position], |previous_positions, distance| {
        let positions = advance_positions(previous_positions, &field, &mut on_loop_positions);

        if distance % frame_every_distance == 0 {
            let mut pixel_map = PixelMap::new((width, height));
            let mut padded_pixel_map = OffsetCanvas::new(&mut pixel_map, (pad, pad));
            draw_field(&mut padded_pixel_map, &field, &on_loop_positions, 4);
            draw_enclosed(&mut padded_pixel_map, &positions, 5);

            font.write_text(
                &mut pixel_map,
                format!("P1 {:4}", distance).as_str(),
                (pad, 455 - pad - font.line_height()),
                2,
            );

            encoder.write(pixel_map.to_vec());
        }

        (positions.len() > 0).then_some(positions)
    });

    let furthest_distance = distances.next().unwrap() - 1;

    println!("Part 1: {:?}", furthest_distance);

    let enclosed_positions = find_enclosed_positions(&field, &on_loop_positions);

    let enclosed_count = enclosed_positions.len();

    for i in (0..enclosed_count).step_by(frame_every_enclosed) {
        let mut pixel_map = PixelMap::new((width, height));
        let mut padded_pixel_map = OffsetCanvas::new(&mut pixel_map, (pad, pad));
        draw_field(&mut padded_pixel_map, &field, &on_loop_positions, 2);
        let i_prev = if i > frame_every_enclosed {
            i - frame_every_enclosed
        } else {
            0
        };
        draw_enclosed(&mut padded_pixel_map, &enclosed_positions[0..i_prev], 4);
        draw_enclosed(&mut padded_pixel_map, &enclosed_positions[i_prev..i], 5);

        font.write_text(
            &mut pixel_map,
            format!("P1 {:4}", furthest_distance).as_str(),
            (pad, 455 - pad - font.line_height()),
            4,
        );

        font.write_text(
            &mut pixel_map,
            format!("        P2 {:4}", i).as_str(),
            (pad, 455 - pad - font.line_height()),
            2,
        );

        encoder.write(pixel_map.to_vec());
    }

    println!("Part 2: {:?}", enclosed_count);

    for _ in 0..50 {
        let mut pixel_map = PixelMap::new((width, height));
        let mut padded_pixel_map = OffsetCanvas::new(&mut pixel_map, (pad, pad));
        draw_field(&mut padded_pixel_map, &field, &on_loop_positions, 2);
        draw_enclosed(&mut padded_pixel_map, &enclosed_positions, 3);

        font.write_text(
            &mut pixel_map,
            format!("P1 {:4}", furthest_distance).as_str(),
            (pad, 455 - pad - font.line_height()),
            4,
        );

        font.write_text(
            &mut pixel_map,
            format!("        P2 {:4}", enclosed_count).as_str(),
            (pad, 455 - pad - font.line_height()),
            5,
        );

        encoder.write(pixel_map.to_vec());
    }

    let elapsed = start.elapsed();
    println!("Took: {:.2?}", elapsed);
}
