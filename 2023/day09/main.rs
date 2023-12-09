use std::{collections::HashMap, ops::Index, vec};

fn parse_sequences(input: &str) -> Vec<Vec<i64>> {
    input
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.split(" ")
                .map(|number_str| number_str.parse().expect(&format!("bad: {number_str}")))
                .collect()
        })
        .collect()
}

#[test]
fn check_parse_sequences() {
    assert_eq!(
        parse_sequences("2 3 -5\n4 7\n\n"),
        vec![vec![2, 3, -5], vec![4, 7]]
    );
}

fn derivatives(sequence: Vec<i64>) -> Vec<Vec<i64>> {
    let mut pyramid = vec![];
    pyramid.push(sequence);
    while !pyramid.last().unwrap().iter().all(|&n| n == 0) {
        let last_sequence = pyramid.last().unwrap();
        let next_sequence = last_sequence
            .windows(2)
            .map(|pair| {
                if let [a, b] = pair {
                    b - a
                } else {
                    panic!("huh?");
                }
            })
            .collect::<Vec<_>>();
        pyramid.push(next_sequence);
    }
    dbg!(pyramid.len());
    pyramid
}

#[test]
fn check_derivatives() {
    assert_eq!(
        derivatives(vec![1, 3, 6, 10, 15, 21]),
        vec![
            vec![1, 3, 6, 10, 15, 21],
            vec![2, 3, 4, 5, 6],
            vec![1, 1, 1, 1],
            vec![0, 0, 0]
        ]
    );
}

fn extrapolate(sequence: Vec<i64>) -> i64 {
    derivatives(sequence)
        .into_iter()
        .map(|d| d.into_iter().last().unwrap())
        .sum()
}

#[test]
fn check_extrapolate() {
    assert_eq!(extrapolate(vec![1, 3, 6, 10, 15, 21]), 28);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day09/input.txt")?;

    let sequences = parse_sequences(&file);
    let extrapolation_sum = sequences.into_iter().map(extrapolate).sum::<i64>();

    println!("Part 1: {:?}", extrapolation_sum);

    Ok(())
}
