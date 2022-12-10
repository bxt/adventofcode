use gif::{Encoder, Frame, Repeat};
use std::cmp::max;
use std::collections::HashSet;
use std::fmt::Error;
use std::fs::File;
use std::str::FromStr;
use std::time::Instant;
use std::{borrow::Cow, cmp::min};
use strum_macros::{EnumIter, EnumString};
use visualisation_utils::font::Font;
use visualisation_utils::pixel_map::PixelMap;

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
    let font = Font::from_file(7, 9, 83, "../2021/visualisation_utils/font.pbm");

    let chain_length = 10;

    let parsed_input = parse_input(&file);
    println!("parsed_input length: {:?}", parsed_input.len());

    let dimensions = figure_dimensions(&parsed_input);

    println!("dimenstions: {:?}", dimensions);

    let (width, height) = (
        u16::try_from(dimensions.0 .1 - dimensions.0 .0).unwrap() + 1,
        u16::try_from(dimensions.1 .1 - dimensions.1 .0).unwrap() + 1,
    );

    println!("width: {:?}, height: {:?}", width, height);

    let color_map = &[
        0x00, 0x11, 0x22, // 0: COLOR_BLUE_1
        0x00, 0x33, 0x66, // 1: COLOR_BLUE_2
        0x00, 0x55, 0xAA, // 2: COLOR_BLUE_3
        0x00, 0x77, 0xEE, // 3: COLOR_BLUE_4
        0x00, 0x99, 0x00, // 4: COLOR_GREEN_3
        0x00, 0xBB, 0x00, // 5: COLOR_GREEN_4
    ];

    let mut image = File::create("day09/output.gif").unwrap();
    let mut encoder = Encoder::new(&mut image, width, height, color_map).unwrap();
    encoder.set_repeat(Repeat::Infinite).unwrap();

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

            let mut pixel_map = PixelMap::new(dimensions, (width, height));

            for &p in &visited_part1 {
                pixel_map.set(p, 1);
            }
            for &p in &visited_part2 {
                pixel_map.set(p, 2);
            }

            pixel_map.set((0, 0), 3);

            for index in 1..chain.len() {
                pixel_map.set(chain[index], 4);
            }

            pixel_map.set(chain[0], 5);

            font.write_text(
                &mut pixel_map,
                format!("P1 {:4}", visited_part1.len()).as_str(),
                (
                    dimensions.0 .0 + 8,
                    dimensions.1 .1 - font.line_height() * 2 - 4,
                ),
                2,
            );

            font.write_text(
                &mut pixel_map,
                format!("P2 {:4}", visited_part2.len()).as_str(),
                (
                    dimensions.0 .0 + 8,
                    dimensions.1 .1 - font.line_height() - 4,
                ),
                3,
            );

            let mut frame = Frame::default();
            frame.width = width;
            frame.height = height;
            frame.buffer = Cow::Owned(pixel_map.to_vec());
            encoder.write_frame(&frame).unwrap();
        }
    }

    let elapsed = start.elapsed();
    println!("Took: {:.2?}s", elapsed);

    Ok(())
}
