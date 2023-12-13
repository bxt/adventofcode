fn parse_pattern(input: &str) -> Vec<&str> {
    input
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .collect()
}

fn potential_mirroring(len: usize) -> impl Iterator<Item = Vec<(usize, usize)>> {
    (1..len).map(move |center| {
        let start_at_0 = center <= len / 2;
        let extend = if start_at_0 { center } else { len - center };
        (0..extend)
            .map(|index| (center - index - 1, center + index))
            .collect()
    })
}

#[test]
fn check_potential_mirroring() {
    assert_eq!(
        potential_mirroring(2).collect::<Vec<_>>(),
        vec![vec![(0, 1)]]
    );
    assert_eq!(
        potential_mirroring(3).collect::<Vec<_>>(),
        vec![vec![(0, 1)], vec![(1, 2)]]
    );
    assert_eq!(
        potential_mirroring(4).collect::<Vec<_>>(),
        vec![vec![(0, 1)], vec![(1, 2), (0, 3),], vec![(2, 3)]]
    );
    assert_eq!(
        potential_mirroring(5).collect::<Vec<_>>(),
        vec![
            vec![(0, 1)],
            vec![(1, 2), (0, 3)],
            vec![(2, 3), (1, 4)],
            vec![(3, 4)]
        ]
    );
    assert_eq!(
        potential_mirroring(6).collect::<Vec<_>>(),
        vec![
            vec![(0, 1)],
            vec![(1, 2), (0, 3)],
            vec![(2, 3), (1, 4), (0, 5)],
            vec![(3, 4), (2, 5)],
            vec![(4, 5)]
        ]
    );
}

fn horizontal_mirror_position(pattern: &Vec<&str>) -> Option<usize> {
    for (center_minus_one, checks) in potential_mirroring(pattern.len()).enumerate() {
        if checks.into_iter().all(|(a, b)| pattern[a] == pattern[b]) {
            return Some(center_minus_one + 1);
        }
    }
    None
}

fn vertical_mirror_position(pattern: &Vec<&str>) -> Option<usize> {
    for (center_minus_one, checks) in potential_mirroring(pattern[0].len()).enumerate() {
        if checks.into_iter().all(|(a, b)| {
            pattern
                .iter()
                .all(|line| line.bytes().nth(a) == line.bytes().nth(b))
        }) {
            return Some(center_minus_one + 1);
        }
    }
    None
}

#[test]
fn check_mirror_positions() {
    let pattern1 = "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.";
    let pattern2 = "#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#";
    assert_eq!(horizontal_mirror_position(&parse_pattern(pattern1)), None);
    assert_eq!(
        horizontal_mirror_position(&parse_pattern(pattern2)),
        Some(4)
    );
    assert_eq!(vertical_mirror_position(&parse_pattern(pattern1)), Some(5));
    assert_eq!(vertical_mirror_position(&parse_pattern(pattern2)), None);
}

fn main() -> () {
    let file = std::fs::read_to_string("day13/input.txt").unwrap();

    let data = file
        .split("\n\n")
        .map(parse_pattern)
        .collect::<Vec<Vec<_>>>();

    let horizontal_mirror_sum = data
        .iter()
        .map(horizontal_mirror_position)
        .map(Option::unwrap_or_default)
        .sum::<usize>();

    let vertical_mirror_sum = data
        .iter()
        .map(vertical_mirror_position)
        .map(Option::unwrap_or_default)
        .sum::<usize>();

    let part1 = vertical_mirror_sum + 100 * horizontal_mirror_sum;
    println!("Part 1: {:?}", part1);
}
