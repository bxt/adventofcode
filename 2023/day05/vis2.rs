use std::cmp::{max, min};
use std::iter::once;
use std::time::Instant;
use visualisation_utils::canvas::{Canvas, OffsetCanvas, PixelMap};
use visualisation_utils::encoder::LoopEncoder;
use visualisation_utils::font::Font;

fn is_empty((start, end, _): &(i64, i64, i64)) -> bool {
    start < end
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

    let seed_str = chunks.first().unwrap().first().unwrap();
    let seeds = seed_str
        .strip_prefix("seeds: ")
        .unwrap()
        .split(" ")
        .map(|s| s.parse::<i64>().unwrap())
        .collect::<Vec<_>>();

    let stages = chunks
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
                    let start = source;
                    let offset = destination - source;
                    let end = source + length;
                    (start, end, offset)
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let stage_count = stages.len();

    let highest_number_i64 = stages
        .iter()
        .flatten()
        .flat_map(|(s, d, l)| once(s + l).chain(once(d + l)))
        .max()
        .unwrap();
    let highest_number = highest_number_i64 as f64;

    let stage_width = 96;
    let frame_every = 4;
    let stage_width_i64 = i64::try_from(stage_width).unwrap();
    let field_height_usize = 300;
    let field_height = field_height_usize as f64;

    let (width, height) = (
        stage_count * stage_width + pad * 2,
        field_height_usize + pad * 3 + font.line_height(),
    );

    let map_y = |y_orig: i64| ((y_orig as f64) * field_height / highest_number).floor() as usize;

    println!("width: {:?}, height: {:?}", width, height);

    let initial_seed_intervals = seeds
        .chunks_exact(2)
        .map(|chunk| {
            if let &[source, length] = chunk {
                let start = source;
                let end = source + length;
                let movement = 0;
                vec![(start, end, movement)]
            } else {
                panic!("at the Disco")
            }
        })
        .collect::<Vec<_>>();

    let mut all_seed_intervals = initial_seed_intervals
        .into_iter()
        .map(|initial_intervals| {
            once(&vec![])
                .chain(stages.iter())
                .scan(initial_intervals, |intervals, mappings| {
                    let mut mapped_intervals = vec![];
                    for &(start_m, end_m, offset) in mappings {
                        let mut unmapped_intervals = vec![];
                        while let Some((start_i, end_i, _)) = intervals.pop() {
                            let before = (start_i, min(end_i, start_m), 0);
                            let intersection = (
                                max(start_i, start_m) + offset,
                                min(end_m, end_i) + offset,
                                offset,
                            );
                            let after = (max(end_m, start_i), end_i, 0);
                            unmapped_intervals.push(before);
                            mapped_intervals.push(intersection);
                            unmapped_intervals.push(after);
                        }
                        intervals.extend(unmapped_intervals.iter().filter(|i| is_empty(i)));
                    }
                    intervals.extend(mapped_intervals.iter().filter(|i| is_empty(i)));
                    Some(intervals.to_vec())
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    all_seed_intervals.sort_by_key(|seeds| -seeds[stage_count].iter().map(|i| i.0).min().unwrap());

    let mut encoder = LoopEncoder::new("day05/output2.gif", (width, height));

    for all_seed_intervals_index in 0..all_seed_intervals.len() {
        for stage in 1..stage_count + 1 {
            let is_last =
                all_seed_intervals_index == all_seed_intervals.len() - 1 && stage == stage_count;
            let addition = if is_last { 3 * stage_width } else { 0 };
            for frame in (0..stage_width + addition).step_by(frame_every) {
                let is_in_addition = frame > stage_width;
                let frame_offset = if is_in_addition { stage_width } else { frame };

                let mut pixel_map = PixelMap::new((width, height));

                for stage_inner in 0..stage_count + 1 {
                    let interval_count =
                        all_seed_intervals_index + if stage_inner <= stage { 1 } else { 0 };
                    if interval_count > 0 {
                        let amount = all_seed_intervals
                            .iter()
                            .take(interval_count)
                            .map(|i| i[stage_inner].len())
                            .sum::<usize>();
                        let x = if stage_inner == 0 {
                            2
                        } else {
                            2 * pad + stage_inner * stage_width - 34
                        };
                        font.write_text(
                            &mut pixel_map,
                            format!("{amount:3}").as_str(),
                            (x, height - 2 * pad - 2 * font.line_height()),
                            1,
                        );
                    }
                }

                let mut padded_pixel_map = OffsetCanvas::new(&mut pixel_map, (pad, pad));

                for stage in 1..stage_count {
                    for y in 0..field_height_usize {
                        padded_pixel_map.set((stage * stage_width, y), 1);
                    }
                }

                for seed_index in 0..all_seed_intervals_index {
                    for stage_inner in 0..stage_count {
                        for x in 0..stage_width {
                            let x_i64 = i64::try_from(x).unwrap();
                            for &(start, end, movement) in
                                &all_seed_intervals[seed_index][stage_inner + 1]
                            {
                                let interpolate =
                                    |y| y + movement * x_i64 / stage_width_i64 - movement;
                                for y in map_y(interpolate(start))..map_y(interpolate(end)) {
                                    padded_pixel_map.set((x + stage_inner * stage_width, y), 1);
                                }
                            }
                        }
                    }
                }

                for seed_index in 0..all_seed_intervals_index {
                    for stage_inner in 0..stage_count {
                        for x in 0..stage_width {
                            let x_i64 = i64::try_from(x).unwrap();
                            for &(start, end, movement) in
                                &all_seed_intervals[seed_index][stage_inner + 1]
                            {
                                let interpolate =
                                    |y| y + movement * x_i64 / stage_width_i64 - movement;
                                let y_start = map_y(interpolate(start));
                                let y_end = map_y(interpolate(end));
                                padded_pixel_map.set((x + stage_inner * stage_width, y_end), 2);
                                padded_pixel_map.set((x + stage_inner * stage_width, y_start), 3);
                            }
                        }
                    }
                }

                for stage_inner in 1..stage + 1 {
                    let amount = all_seed_intervals[all_seed_intervals_index][stage_inner].len();
                    let x = 2 * pad + stage_inner * stage_width - 34;
                    font.write_text(&mut pixel_map, format!("{amount:3}").as_str(), (x, pad), 4);
                }

                let mut padded_pixel_map = OffsetCanvas::new(&mut pixel_map, (pad, pad));

                for stage_inner in 0..stage {
                    let is_last = stage_inner == stage - 1;
                    let x_limit = if is_last { frame_offset } else { stage_width };
                    for x in 0..x_limit {
                        let x_i64 = i64::try_from(x).unwrap();
                        for &(start, end, movement) in
                            &all_seed_intervals[all_seed_intervals_index][stage_inner + 1]
                        {
                            let interpolate = |y| y + movement * x_i64 / stage_width_i64 - movement;
                            for y in map_y(interpolate(start))..map_y(interpolate(end)) {
                                padded_pixel_map.set((x + stage_inner * stage_width, y), 4);
                            }
                        }
                    }
                }

                for stage_inner in 0..stage {
                    let is_last = stage_inner == stage - 1;
                    let x_limit = if is_last { frame_offset } else { stage_width };
                    for x in 0..x_limit {
                        let x_i64 = i64::try_from(x).unwrap();
                        for &(start, end, movement) in
                            &all_seed_intervals[all_seed_intervals_index][stage_inner + 1]
                        {
                            let interpolate = |y| y + movement * x_i64 / stage_width_i64 - movement;
                            let y_start = map_y(interpolate(start));
                            let y_end = map_y(interpolate(end));
                            padded_pixel_map.set((x + stage_inner * stage_width, y_end), 5);
                            padded_pixel_map.set((x + stage_inner * stage_width, y_start), 5);
                        }
                    }
                }

                let min_seed_so_far = all_seed_intervals
                    .iter()
                    .take(all_seed_intervals_index + if is_in_addition { 1 } else { 0 })
                    .flat_map(|seeds| seeds[stage_count].iter().map(|x| x.0))
                    .min()
                    .unwrap_or(highest_number_i64);

                font.write_text(
                    &mut pixel_map,
                    format!("P2 {:10}", min_seed_so_far).as_str(),
                    (pad, height - pad - font.line_height()),
                    if is_in_addition { 4 } else { 2 },
                );

                if frame % frame_every == 0 {
                    encoder.write(pixel_map.to_vec());
                }
            }
        }
    }

    let elapsed = start.elapsed();
    println!("Took: {:.2?}", elapsed);

    Ok(())
}
