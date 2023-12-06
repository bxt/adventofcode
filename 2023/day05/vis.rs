use std::cmp::{max, min};
use std::iter::once;
use std::time::Instant;
use visualisation_utils::canvas::{Canvas, OffsetCanvas, PixelMap};
use visualisation_utils::encoder::LoopEncoder;
use visualisation_utils::font::Font;

fn use_mapping((s, d, _): &(i64, i64, i64), seed: &i64) -> i64 {
    seed - s + d
}

fn use_any_mapping(mapping: &Vec<(i64, i64, i64)>, seed: &i64) -> i64 {
    let applied_mapping = mapping
        .iter()
        .find(|(s, _, l)| seed >= s && seed < &(s + l))
        .unwrap_or(&(0, 0, 0));
    use_mapping(applied_mapping, seed)
}

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
            let mut mapping = chunk
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
                .collect::<Vec<_>>();
            mapping.sort();
            mapping
        })
        .collect::<Vec<_>>();

    let highest_seed_number = *seeds.iter().max().unwrap();
    let highest_range_number = mappings
        .iter()
        .flatten()
        .flat_map(|(s, d, l)| once(s + l).chain(once(d + l)))
        .max()
        .unwrap();
    let highest_number_i64 = max(highest_seed_number, highest_range_number);
    let highest_number = highest_number_i64 as f64;

    let mapping_width = 96;
    let mapping_width_i64 = i64::try_from(mapping_width).unwrap();
    let field_height_usize = 200;
    let field_height = field_height_usize as f64;

    let (width, height) = (
        mappings.len() * mapping_width + pad * 2,
        field_height_usize * 2 + pad * 5 + font.line_height() * 2,
    );

    println!("width: {:?}, height: {:?}", width, height);

    let mut seed_positions = vec![seeds.to_vec()];
    for mapping in mappings.iter() {
        seed_positions.push(
            seed_positions
                .last()
                .unwrap()
                .iter()
                .map(|seed| use_any_mapping(mapping, seed))
                .collect(),
        );
    }

    let seed_ranges = seeds
        .chunks(2)
        .map(|chunk| {
            if let &[source, length] = chunk {
                ((-1, source), length)
            } else {
                panic!("at the Disco")
            }
        })
        .collect::<Vec<_>>();

    let mut seed_ranges_list = vec![seed_ranges];

    for mapping in mappings.iter() {
        seed_ranges_list.push(
            seed_ranges_list
                .last()
                .unwrap()
                .iter()
                .flat_map(|((_, end), length)| {
                    let mut results = vec![];
                    let mut start_unmapped = *end;
                    let mut length_unmapped = *length;
                    let mut mapping_index = 0;
                    while mapping_index < mapping.len() && length_unmapped > 0 {
                        let map = mapping[mapping_index];
                        let (source, _, length) = map;
                        if source + length <= start_unmapped {
                            mapping_index += 1;
                        } else if source > start_unmapped {
                            let mapped_length = min(length_unmapped, source - start_unmapped);
                            results.push(((start_unmapped, start_unmapped), mapped_length));
                            length_unmapped -= mapped_length;
                            start_unmapped += mapped_length;
                        } else {
                            let mapped_length =
                                min(length_unmapped, length - (start_unmapped - source));
                            let dest = use_mapping(&map, &start_unmapped);
                            results.push(((start_unmapped, dest), mapped_length));
                            length_unmapped -= mapped_length;
                            start_unmapped += mapped_length;
                        }
                    }
                    if length_unmapped > 0 {
                        results.push(((start_unmapped, start_unmapped), length_unmapped));
                    }
                    results
                })
                .collect(),
        );
    }

    let mut encoder = LoopEncoder::new("day05/output.gif", (width, height));

    let map_y = |y_orig: i64| ((y_orig as f64) * field_height / highest_number).floor() as usize;
    let offset_p2 = field_height_usize + pad * 2 + font.line_height();

    let mut seeds_interpolated_history = vec![];
    let mut seed_ranges_list_interpolated_history = vec![];

    for mapping_index in 0..mappings.len() {
        let addition = if mapping_index == mappings.len() - 1 {
            mapping_width
        } else {
            0
        };
        for frame in 0..mapping_width + addition {
            let is_in_addition = frame > mapping_width;
            let frame_offset = if is_in_addition { mapping_width } else { frame };
            let frame_offset_i64 = i64::try_from(frame_offset).unwrap();

            let mut pixel_map = PixelMap::new((width, height));
            let mut padded_pixel_map = OffsetCanvas::new(&mut pixel_map, (pad, pad));

            for mapping_index in 1..mappings.len() {
                for y in 0..field_height_usize {
                    padded_pixel_map.set((mapping_index * mapping_width, y), 1);
                    padded_pixel_map.set((mapping_index * mapping_width, y + offset_p2), 1);
                }
            }

            let interpolate =
                |(seed1, seed2)| seed1 + (seed2 - seed1) * frame_offset_i64 / mapping_width_i64;

            let seeds_interpolated = seed_positions[mapping_index]
                .iter()
                .zip(seed_positions[mapping_index + 1].iter())
                .map(interpolate)
                .collect::<Vec<_>>();

            let min_seed = *seeds_interpolated.iter().min().unwrap();

            if !is_in_addition {
                seeds_interpolated_history.push(seeds_interpolated);
            }

            for (offset, seeds_interpolated) in seeds_interpolated_history.iter().enumerate() {
                let min_seed = *seeds_interpolated.iter().min().unwrap();

                for seed in seeds_interpolated {
                    let is_last_index = offset == seeds_interpolated_history.len() - 1;
                    let value = if is_last_index { 4 } else { 3 };
                    padded_pixel_map.set((offset, map_y(*seed)), value);
                }

                padded_pixel_map.set((offset, map_y(min_seed)), 5);
            }

            font.write_text(
                &mut pixel_map,
                format!("P1 {:10}", min_seed).as_str(),
                (pad, height - pad - font.line_height() - offset_p2),
                if is_in_addition { 4 } else { 2 },
            );

            for mapping_index in 0..mapping_index + 2 {
                let amount = seed_ranges_list[mapping_index].len();
                let x = if mapping_index == 0 {
                    2
                } else {
                    2 * pad + mapping_index * mapping_width - 34
                };
                font.write_text(
                    &mut pixel_map,
                    format!("{amount:3}").as_str(),
                    (x, height - 2 * pad - 2 * font.line_height()),
                    1,
                );
            }

            let mut padded_pixel_map_p2 = OffsetCanvas::new(&mut pixel_map, (pad, pad + offset_p2));

            let seed_ranges_interpolated = seed_ranges_list[mapping_index + 1]
                .iter()
                .map(|((from, to), length)| (interpolate((from, to)), *length))
                .collect::<Vec<_>>();

            let min_seed_p2 = seed_ranges_interpolated.iter().map(|r| r.0).min().unwrap();

            if !is_in_addition {
                seed_ranges_list_interpolated_history.push(seed_ranges_interpolated);
            }

            for (offset, seed_ranges_interpolated) in
                seed_ranges_list_interpolated_history.iter().enumerate()
            {
                for &(from, length) in seed_ranges_interpolated {
                    let end = from + length;

                    for y in map_y(from)..map_y(end) {
                        padded_pixel_map_p2.set((offset, y), 1);
                    }
                }
            }

            for (offset, seed_ranges_interpolated) in
                seed_ranges_list_interpolated_history.iter().enumerate()
            {
                let min_seed = seed_ranges_interpolated.iter().map(|r| r.0).min().unwrap();

                for &(from, length) in seed_ranges_interpolated {
                    let is_last_index = offset == seed_ranges_list_interpolated_history.len() - 1;
                    let end = from + length;

                    let value = if is_last_index { 4 } else { 3 };
                    padded_pixel_map_p2.set((offset, map_y(from)), value);
                    padded_pixel_map_p2.set((offset, map_y(end)), 2);
                }

                padded_pixel_map_p2.set((offset, map_y(min_seed)), 5);
            }

            font.write_text(
                &mut pixel_map,
                format!("P2 {:10}", min_seed_p2).as_str(),
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
