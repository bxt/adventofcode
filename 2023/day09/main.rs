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

fn extrapolate_backwards(sequence: Vec<i64>) -> i64 {
    let first_values = derivatives(sequence).into_iter().rev().map(|d| d[0]);
    first_values.fold(0, |acc, first_value| first_value - acc)
}

#[test]
fn check_extrapolate_backwards() {
    assert_eq!(extrapolate_backwards(vec![10, 13, 16, 21, 30, 45]), 5);
    assert_eq!(extrapolate_backwards(vec![0, 3, 6, 9, 12, 15]), -3);
    assert_eq!(extrapolate_backwards(vec![1, 3, 6, 10, 15, 21]), 0);
}

#[test]
fn check_extrapolate_backwards_invariant() {
    let file = std::fs::read_to_string("day09/input.txt").unwrap();
    let sequences = parse_sequences(&file);
    for sequence in sequences {
        let initial_pyramid = derivatives(sequence.to_vec());
        let initial_pyramid_size = initial_pyramid.len();
        let backwards_value = extrapolate_backwards(sequence.to_vec());
        let backwards_pyramid_size =
            derivatives(std::iter::once(backwards_value).chain(sequence).collect()).len();
        assert_eq!(initial_pyramid_size, backwards_pyramid_size);
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day09/input.txt")?;

    let sequences = parse_sequences(&file);
    let extrapolation_sum = sequences
        .iter()
        .map(|s| extrapolate(s.to_vec()))
        .sum::<i64>();

    println!("Part 1: {:?}", extrapolation_sum);

    let extrapolation_backwards_sum = sequences
        .into_iter()
        .map(extrapolate_backwards)
        .sum::<i64>();

    println!("Part 2: {:?}", extrapolation_backwards_sum);

    Ok(())
}
