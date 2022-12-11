use std::cmp::max;
use std::cmp::min;
use std::collections::HashSet;
use std::fmt::Error;
use std::str::FromStr;
use std::time::Instant;
use strum_macros::{EnumIter, EnumString};
use visualisation_utils::encoder::LoopEncoder;
use visualisation_utils::font::get_font;
use visualisation_utils::pixel_map::{Canvas, MappedCanvas, OffsetCanvas, PixelMap};

#[derive(Debug, EnumString, EnumIter)]
enum Direction {
    L,
    R,
    U,
    D,
}

#[derive(Debug)]
struct Move {
    direction: Direction,
    amount: u32,
}

impl FromStr for Move {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let (a, b) = input.split_once(" ").unwrap();

        Ok(Self {
            direction: a.parse().unwrap(),
            amount: b.parse().unwrap(),
        })
    }
}

trait Movable {
    fn move_once(&mut self, direction: &Direction);
}

impl Movable for (i32, i32) {
    fn move_once(&mut self, direction: &Direction) {
        match direction {
            Direction::L => {
                self.0 -= 1;
            }
            Direction::R => {
                self.0 += 1;
            }
            Direction::U => {
                self.1 -= 1;
            }
            Direction::D => {
                self.1 += 1;
            }
        }
    }
}

impl Movable for Vec<(i32, i32)> {
    fn move_once(&mut self, direction: &Direction) {
        self[0].move_once(direction);

        for index in 1..self.len() {
            let (head_x, head_y) = self[index - 1];
            let (tail_x, tail_y) = &mut self[index];

            let tail_diff_x = head_x - *tail_x;
            let tail_diff_y = head_y - *tail_y;
            if tail_diff_x.abs() == 2 {
                if tail_diff_y.abs() == 2 {
                    *tail_x += tail_diff_x / 2;
                    *tail_y += tail_diff_y / 2;
                } else {
                    *tail_x += tail_diff_x / 2;
                    *tail_y += tail_diff_y;
                }
            } else if tail_diff_y.abs() == 2 {
                *tail_x += tail_diff_x;
                *tail_y += tail_diff_y / 2;
            }
        }
    }
}

fn parse_input(input: &str) -> Vec<Move> {
    input
        .trim()
        .lines()
        .map(|line| line.parse().unwrap())
        .collect::<Vec<_>>()
}

fn figure_dimensions(input: &Vec<Move>) -> ((i32, i32), (i32, i32)) {
    let mut position: (i32, i32) = (0, 0);
    let mut dimensions: ((i32, i32), (i32, i32)) = ((0, 0), (0, 0));

    for Move { direction, amount } in input {
        for _ in 0..*amount {
            position.move_once(direction);
            dimensions.0 .0 = min(dimensions.0 .0, position.0);
            dimensions.0 .1 = max(dimensions.0 .1, position.0);
            dimensions.1 .0 = min(dimensions.1 .0, position.1);
            dimensions.1 .1 = max(dimensions.1 .1, position.1);
        }
    }

    dimensions
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let start = Instant::now();

    let file = std::fs::read_to_string("day09/input.txt")?;
    let font = get_font();

    let chain_length = 10;

    let parsed_input = parse_input(&file);
    println!("parsed_input length: {:?}", parsed_input.len());

    let dimensions = figure_dimensions(&parsed_input);

    println!("dimenstions: {:?}", dimensions);

    let (width, height) = (
        usize::try_from(dimensions.0 .1 - dimensions.0 .0).unwrap() + 1,
        usize::try_from(dimensions.1 .1 - dimensions.1 .0).unwrap() + 1,
    );

    println!("width: {:?}, height: {:?}", width, height);

    let mut encoder = LoopEncoder::new("day09/output.gif", (width, height));

    let mut visited_part1: HashSet<(i32, i32)> = HashSet::new();
    let mut visited_part2: HashSet<(i32, i32)> = HashSet::new();
    let mut chain: Vec<(i32, i32)> = vec![(0, 0); chain_length];

    for (input_index, Move { direction, amount }) in parsed_input.iter().enumerate() {
        if input_index % 10 == 0 {
            println!("Move {} / {}...", input_index, parsed_input.len());
        }

        for _ in 0..*amount {
            chain.move_once(&direction);
            visited_part1.insert(chain[1]);
            visited_part2.insert(*chain.last().unwrap());

            let mut pixel_map = PixelMap::new((width.into(), height.into()));

            {
                let mut mapped_canvas: MappedCanvas<i32, _> = MappedCanvas::new(&mut pixel_map);
                let mut canvas =
                    OffsetCanvas::new(&mut mapped_canvas, (-dimensions.0 .0, -dimensions.1 .0));

                for &p in &visited_part1 {
                    canvas.set(p, 1);
                }
                for &p in &visited_part2 {
                    canvas.set(p, 2);
                }

                canvas.set((0, 0), 3);

                for index in 1..chain.len() {
                    canvas.set(chain[index], 4);
                }

                canvas.set(chain[0], 5);
            }

            font.write_text(
                &mut pixel_map,
                format!("P1 {:4}", visited_part1.len()).as_str(),
                (8, height - font.line_height() * 2 - 4),
                2,
            );

            font.write_text(
                &mut pixel_map,
                format!("P2 {:4}", visited_part2.len()).as_str(),
                (8, height - font.line_height() - 4),
                3,
            );

            encoder.write(pixel_map.to_vec());
        }
    }

    let elapsed = start.elapsed();
    println!("Took: {:.2?}", elapsed);

    Ok(())
}
