use std::iter::once;
use std::time::Instant;
use visualisation_utils::canvas::{Canvas, OffsetCanvas, PixelMap};
use visualisation_utils::encoder::LoopEncoder;
use visualisation_utils::font::Font;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let start = Instant::now();

    let file = std::fs::read_to_string("day05/input.txt")?;
    let font = Font::from_file(7, 9, 83, "../2021/visualisation_utils/font.pbm");
    let pad = 8;

    let chunks = file
        .split("\n\n")
        .map(|chunk| {
            chunk
                .split("\n")
                .map(|line| line.trim())
                .filter(|line| !line.is_empty())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let seeds = chunks
        .first()
        .unwrap()
        .first()
        .unwrap()
        .strip_prefix("seeds: ")
        .unwrap()
        .split(" ")
        .map(|s| s.parse::<i64>().unwrap())
        .collect::<Vec<_>>();

    let mappings = chunks
        .iter()
        .skip(1)
        .map(|chunk| {
            chunk
                .iter()
                .skip(1)
                .map(|line| {
                    let (one, two_three) = line.split_once(" ").unwrap();
                    let (two, three) = two_three.split_once(" ").unwrap();
                    let destination = one.parse::<i64>().unwrap();
                    let source = two.parse::<i64>().unwrap();
                    let length = three.parse::<i64>().unwrap();
                    (source, destination, length)
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let highest_seed_number = *seeds.iter().max().unwrap();
    let highest_range_number = mappings
        .iter()
        .flatten()
        .flat_map(|(s, d, l)| once(s + l).chain(once(d + l)))
        .max()
        .unwrap();
    let highest_number_i64 = once(highest_seed_number)
        .chain(once(highest_range_number))
        .max()
        .unwrap();
    let highest_number = highest_number_i64 as f64;

    let mapping_width = 50;
    let mapping_width_i64 = i64::try_from(mapping_width).unwrap();
    let field_height_usize = 100;
    let field_height = field_height_usize as f64;

    let (width, height) = (
        mappings.len() * mapping_width + pad * 2,
        field_height_usize + pad * 3 + font.line_height(),
    );

    println!("width: {:?}, height: {:?}", width, height);

    let mut seed_positions = vec![seeds];
    for mapping in mappings.iter() {
        seed_positions.push(
            seed_positions
                .last()
                .unwrap()
                .iter()
                .map(|seed| {
                    let applied_mapping = mapping
                        .iter()
                        .find(|(s, _, l)| seed >= s && seed < &(s + l))
                        .unwrap_or(&(0, 0, 0));
                    let (s, d, _) = applied_mapping;
                    seed - s + d
                })
                .collect(),
        );
    }

    let mut encoder = LoopEncoder::new("day05/output.gif", (width, height));

    let map_y = |y_orig: i64| ((y_orig as f64) * field_height / highest_number).floor() as usize;

    for mapping_index in 0..mappings.len() {
        let mapping_offset = mapping_index * mapping_width;
        let addition = if mapping_index == mappings.len() - 1 {
            mapping_width
        } else {
            0
        };
        for frame in 0..mapping_width + addition {
            let is_in_addition = frame > mapping_width;
            let frame_offset = if is_in_addition { mapping_width } else { frame };
            let frame_offset_i64 = i64::try_from(frame_offset).unwrap();
            let offset = mapping_offset + frame_offset;

            let mut pixel_map = PixelMap::new((width, height));
            let mut padded_pixel_map = OffsetCanvas::new(&mut pixel_map, (pad, pad));

            for mapping_index in 1..mappings.len() {
                for y in 0..field_height_usize {
                    padded_pixel_map.set((mapping_index * mapping_width, y), 1);
                }
            }

            // color 1: BG
            // color 2: BG overlapping
            // color 3: range bounds
            // color 4: seeds
            // color 5: min seed

            let seeds_interpolated = seed_positions[mapping_index]
                .iter()
                .zip(seed_positions[mapping_index + 1].iter())
                .map(|(seed1, seed2)| {
                    seed1 + (seed2 - seed1) * frame_offset_i64 / mapping_width_i64
                })
                .collect::<Vec<_>>();

            let min_seed = *seeds_interpolated.iter().min().unwrap();

            for seed in seeds_interpolated {
                padded_pixel_map.set((offset, map_y(seed)), 4);
            }

            padded_pixel_map.set((offset, map_y(min_seed)), 5);

            font.write_text(
                &mut pixel_map,
                format!("P1 {:10}", min_seed).as_str(),
                (pad, height - pad - font.line_height()),
                if is_in_addition { 4 } else { 2 },
            );

            encoder.write(pixel_map.to_vec());
        }
    }

    let elapsed = start.elapsed();
    println!("Took: {:.2?}", elapsed);

    Ok(())
}
