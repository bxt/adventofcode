use std::iter::once;
use std::{collections::HashSet, vec};

use strum::IntoEnumIterator;
use strum_macros::{EnumIter, EnumString};

use visualisation_utils::canvas::{Canvas, PixelMap};
use visualisation_utils::encoder::LoopEncoder;
use visualisation_utils::font::get_font;

#[derive(Debug, Clone, Copy, PartialEq, EnumString, EnumIter)]
enum Heading {
    #[strum(serialize = ">")]
    Right,
    #[strum(serialize = "v")]
    Down,
    #[strum(serialize = "<")]
    Left,
    #[strum(serialize = "^")]
    Up,
}

impl Heading {
    fn go(&self, (x, y): (i32, i32)) -> (i32, i32) {
        match self {
            Self::Right => (x + 1, y),
            Self::Down => (x, y + 1),
            Self::Left => (x - 1, y),
            Self::Up => (x, y - 1),
        }
    }
}

#[derive(Debug)]
struct Blizzard {
    x: i32,
    y: i32,
    heading: Heading,
}

impl Blizzard {
    fn step(&mut self, (width, height): (i32, i32)) {
        let Blizzard { x, y, heading } = self;
        let (mut new_x, mut new_y) = heading.go((*x, *y));
        if new_x < 0 {
            new_x = width - 1
        }
        if new_x >= width {
            new_x = 0
        }
        if new_y < 0 {
            new_y = height - 1
        }
        if new_y >= height {
            new_y = 0
        }
        *x = new_x;
        *y = new_y;
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = std::fs::read_to_string("day24/input.txt")?;
    let font = get_font();

    let mut blizzards = vec![];
    let mut start_x = -1;
    let mut end_x = -1;

    let lines = input.trim().lines().collect::<Vec<_>>();

    let width = i32::try_from(lines[0].len()).unwrap() - 2;
    let height = i32::try_from(lines.len()).unwrap() - 2;

    for (y_orig, line) in lines.iter().enumerate() {
        let y = i32::try_from(y_orig).unwrap() - 1;
        for (x_orig, letter) in line.split("").enumerate() {
            let x = i32::try_from(x_orig).unwrap() - 2;
            if y_orig == 0 && letter == "." {
                start_x = x;
            }
            if y_orig == lines.len() - 1 && letter == "." {
                end_x = x;
            }
            if let Ok(heading) = letter.parse() {
                blizzards.push(Blizzard { x, y, heading });
            }
        }
    }

    let mut has_snacks = false;
    let mut is_going_back = false;

    let mut expeditions = HashSet::new();
    expeditions.insert((start_x, -1));

    let mut result_part1 = 0;
    let mut result_part2 = 0;

    let pad = 12;
    let pad_i32 = i32::try_from(pad).unwrap();
    let (canvas_width, canvas_height) = (
        usize::try_from(width).unwrap() * 4 + pad * 2,
        usize::try_from(height).unwrap() * 4 + pad * 3 + font.line_height(),
    );
    let mut encoder = LoopEncoder::new("day24/output.gif", (canvas_width, canvas_height));

    loop {
        let mut occupied_spots = HashSet::new();

        for blizzard in blizzards.iter_mut() {
            blizzard.step((width, height));
            occupied_spots.insert((blizzard.x, blizzard.y));
        }

        expeditions = expeditions
            .iter()
            .flat_map(|expedition| {
                Heading::iter()
                    .map(|heading| heading.go(*expedition))
                    .chain(once(*expedition))
                    .filter_map(|moved| {
                        let (x, y) = moved;
                        let is_startpoint = y == -1 && x == start_x;
                        let is_endpoint = y == height && x == end_x;
                        let is_in_field = 0 <= x && x < width && 0 <= y && y < height;
                        let is_in_bounds = is_startpoint || is_endpoint || is_in_field;
                        let is_on_blizzard = occupied_spots.contains(&moved);
                        (!is_on_blizzard && is_in_bounds).then_some(moved)
                    })
            })
            .collect();

        let target_y = if is_going_back { -1 } else { height };

        result_part2 += 1;
        if !is_going_back && !has_snacks {
            result_part1 += 1;
        }

        for subframe in 0..4 {
            let mut pixel_map = PixelMap::new((canvas_width, canvas_height));

            for blizzard in &blizzards {
                let mut pos = (blizzard.x * 4, blizzard.y * 4);
                for _ in 0..subframe {
                    pos = blizzard.heading.go(pos);
                }
                let (x, y) = (
                    usize::try_from(pos.0 + pad_i32).unwrap(),
                    usize::try_from(pos.1 + pad_i32).unwrap(),
                );
                pixel_map.set((x, y), 3);
                match blizzard.heading {
                    Heading::Right => {
                        pixel_map.set((x - 1, y - 1), 3);
                        pixel_map.set((x - 1, y + 1), 3);
                    }
                    Heading::Down => {
                        pixel_map.set((x - 1, y - 1), 3);
                        pixel_map.set((x + 1, y - 1), 3);
                    }
                    Heading::Left => {
                        pixel_map.set((x + 1, y - 1), 3);
                        pixel_map.set((x + 1, y + 1), 3);
                    }
                    Heading::Up => {
                        pixel_map.set((x - 1, y + 1), 3);
                        pixel_map.set((x + 1, y + 1), 3);
                    }
                }
            }
            for expedition in &expeditions {
                let (x, y) = (
                    usize::try_from(expedition.0 * 4 + pad_i32).unwrap(),
                    usize::try_from(expedition.1 * 4 + pad_i32).unwrap(),
                );
                pixel_map.set((x, y), 5);
                pixel_map.set((x + 1, y), 4);
                pixel_map.set((x, y + 1), 4);
                pixel_map.set((x - 1, y), 4);
                pixel_map.set((x, y - 1), 4);
            }

            let text_part1 = format!("P1 {:3}", result_part1);
            font.write_text(
                &mut pixel_map,
                text_part1.as_str(),
                (pad, canvas_height - font.line_height() - pad),
                2,
            );
            font.write_text(
                &mut pixel_map,
                format!("{} P2 {:3}", " ".repeat(text_part1.len()), result_part2).as_str(),
                (pad, canvas_height - font.line_height() - pad),
                3,
            );

            encoder.write(pixel_map.to_vec());
        }

        if let Some(&endpoint) = expeditions.iter().find(|(_, y)| *y == target_y) {
            if has_snacks {
                break;
            } else {
                if is_going_back {
                    has_snacks = true;
                }
                is_going_back = !is_going_back;
                expeditions = HashSet::new();
                expeditions.insert(endpoint);
            }
        }
    }

    dbg!(result_part1);
    dbg!(result_part2);

    Ok(())
}
