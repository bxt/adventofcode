use std::collections::HashSet;
use std::time::Instant;
use visualisation_utils::canvas::{Canvas, PixelMap};

use visualisation_utils::encoder::LoopEncoder;
use visualisation_utils::font::Font;

fn parse_input(input: &str) -> Vec<Vec<u32>> {
    input
        .trim()
        .lines()
        .map(|line| line.chars().map(|d| d.to_digit(10).unwrap()).collect())
        .collect()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let start = Instant::now();

    let file = std::fs::read_to_string("day08/input.txt")?;
    let font = Font::from_file(7, 9, 83, "../2021/visualisation_utils/font.pbm");
    let pad = 8;

    let trees = parse_input(&file);

    let (width, height) = (
        trees[0].len() + pad * 2,
        trees.len() + pad * 3 + font.line_height(),
    );

    println!("width: {:?}, height: {:?}", width, height);

    let mut encoder = LoopEncoder::new("day08/output.gif", (width, height));

    let mut checks: Vec<Vec<(usize, usize)>> = vec![];

    let rows = trees[0].len();
    let cols = trees.len();
    checks.extend((0..rows).map(|r| (0..cols).map(move |c| (r, c)).collect()));
    checks.extend((0..rows).map(|r| (0..cols).rev().map(move |c| (r, c)).collect()));
    checks.extend((0..cols).map(|c| (0..rows).map(move |r| (r, c)).collect()));
    checks.extend((0..cols).map(|c| (0..rows).rev().map(move |r| (r, c)).collect()));

    let mut visible: HashSet<(usize, usize)> = HashSet::new();

    for check in checks {
        let mut highest_so_far = None;
        let mut visible_this_check: HashSet<(usize, usize)> = HashSet::new();
        for (row_index, col_index) in check {
            let tree = trees[row_index][col_index];
            if highest_so_far.and_then(|x| (tree <= x).then_some(0)) == None {
                highest_so_far = Some(tree);
                let tree_point = (col_index, row_index);
                visible.insert(tree_point);
                visible_this_check.insert(tree_point);

                let mut pixel_map = PixelMap::new((width, height));

                for &p in &visible {
                    pixel_map.set(p, 1);
                }
                for &p in &visible_this_check {
                    pixel_map.set(p, 4);
                }

                pixel_map.set(tree_point, 5);

                font.write_text(
                    &mut pixel_map,
                    format!("P1 {:4}", visible.len()).as_str(),
                    (pad, height - pad - font.line_height()),
                    2,
                );

                encoder.write(pixel_map.to_vec());
            }
        }
    }

    let elapsed = start.elapsed();
    println!("Took: {:.2?}", elapsed);

    Ok(())
}
