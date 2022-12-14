use core::cmp::{max, min};
use std::collections::HashSet;
use std::time::Instant;
use visualisation_utils::canvas::{Canvas, PixelMap};
use visualisation_utils::encoder::LoopEncoder;
use visualisation_utils::font::get_font;

fn parse_input(input: &str) -> Vec<Vec<(usize, usize)>> {
    input
        .lines()
        .map(|line| {
            line.split(" -> ")
                .map(|point| {
                    let (a, b) = point.split_once(",").unwrap();
                    (a.parse().unwrap(), b.parse().unwrap())
                })
                .collect()
        })
        .collect()
}

fn simulate_sand(
    input: &Vec<Vec<(usize, usize)>>,
) -> (HashSet<(usize, usize)>, Vec<(usize, usize)>) {
    let abyss_after = *input
        .iter()
        .flat_map(|line| line.iter().map(|(_, y)| y))
        .max()
        .unwrap();

    let mut rocks = HashSet::new();
    for line in input {
        for window in line.windows(2) {
            match window {
                &[(from_x, from_y), (to_x, to_y)] => {
                    for x in min(from_x, to_x)..=max(from_x, to_x) {
                        for y in min(from_y, to_y)..=max(from_y, to_y) {
                            rocks.insert((x, y));
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    let mut sands = HashSet::new();
    let mut sands_in_order = vec![];
    let start_grain = (500, 0);

    'pouring: loop {
        let mut grain = start_grain;

        while let Some(new_grain) = {
            if grain.1 > abyss_after {
                break 'pouring;
            }

            let (grain_x, grain_y) = grain;
            let candidates = [
                (grain_x, grain_y + 1),
                (grain_x - 1, grain_y + 1),
                (grain_x + 1, grain_y + 1),
            ];
            candidates
                .into_iter()
                .find(|new_grain| !rocks.contains(new_grain) && !sands.contains(new_grain))
        } {
            grain = new_grain;
        }

        sands.insert(grain);
        sands_in_order.push(grain);

        if grain == start_grain {
            break 'pouring;
        }
    }

    (rocks, sands_in_order)
}

fn figure_dimensions(rocks: &HashSet<(usize, usize)>) -> ((usize, usize), (usize, usize)) {
    let mut dimensions: ((usize, usize), (usize, usize)) = ((500, 500), (0, 0));

    for rock in rocks {
        dimensions.0 .0 = min(dimensions.0 .0, rock.0);
        dimensions.0 .1 = max(dimensions.0 .1, rock.0);
        dimensions.1 .0 = min(dimensions.1 .0, rock.1);
        dimensions.1 .1 = max(dimensions.1 .1, rock.1);
    }

    dimensions
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let start = Instant::now();

    let file = std::fs::read_to_string("day14/input.txt")?;
    let font = get_font();

    let parsed_input = parse_input(&file);

    let (rocks, sands_in_order) = simulate_sand(&parsed_input);

    let dimensions = figure_dimensions(&rocks);

    println!("dimensions. {dimensions:?}");

    let (width, height) = (
        dimensions.0 .1 - dimensions.0 .0 + 17,
        dimensions.1 .1 - dimensions.1 .0 + 17 + font.line_height() + 4,
    );

    let adjust = |(x, y): (usize, usize)| -> (usize, usize) {
        (x + 8 - dimensions.0 .0, y + 8 - dimensions.1 .0)
    };

    println!("width: {:?}, height: {:?}", width, height);

    let mut encoder = LoopEncoder::new("day14/output.gif", (width, height));

    for index in 0..sands_in_order.len() {
        let mut pixel_map = PixelMap::new((width.into(), height.into()));

        for &p in &sands_in_order[0..index] {
            pixel_map.set(adjust(p), 1);
        }
        for &p in &rocks {
            pixel_map.set(adjust(p), 2);
        }

        pixel_map.set(adjust((500, 0)), 3);

        pixel_map.set(adjust(sands_in_order[index]), 5);

        font.write_text(
            &mut pixel_map,
            format!("P1 {:3}", index).as_str(),
            (8, height - font.line_height() - 4),
            2,
        );

        encoder.write(pixel_map.to_vec());
    }

    let elapsed = start.elapsed();
    println!("Took: {:.2?}", elapsed);

    Ok(())
}
