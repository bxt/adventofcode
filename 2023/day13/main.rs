fn parse_data(input: &str) -> Vec<&str> {
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

trait Pattern {
    fn len(&self) -> usize;
    fn count_smudges_between(&self, positions: (usize, usize)) -> usize;
}

struct HorizontalPattern<'a>(&'a Vec<&'a str>);

impl<'a> From<&'a Vec<&'a str>> for HorizontalPattern<'a> {
    fn from(value: &'a Vec<&'a str>) -> Self {
        HorizontalPattern(value)
    }
}

impl Pattern for HorizontalPattern<'_> {
    fn len(&self) -> usize {
        self.0.len()
    }

    fn count_smudges_between(&self, (a, b): (usize, usize)) -> usize {
        self.0[a]
            .bytes()
            .zip(self.0[b].bytes())
            .filter(|(a, b)| a != b)
            .count()
    }
}

struct VerticalPattern<'a>(&'a Vec<&'a str>);

impl<'a> From<&'a Vec<&'a str>> for VerticalPattern<'a> {
    fn from(value: &'a Vec<&'a str>) -> Self {
        VerticalPattern(value)
    }
}

impl Pattern for VerticalPattern<'_> {
    fn len(&self) -> usize {
        self.0[0].len()
    }

    fn count_smudges_between(&self, (a, b): (usize, usize)) -> usize {
        self.0
            .iter()
            .filter(|line| line.bytes().nth(a) != line.bytes().nth(b))
            .count()
    }
}

fn mirror_position(pattern: impl Pattern, expected_smudges: usize) -> Option<usize> {
    for (center_minus_one, checks) in potential_mirroring(pattern.len()).enumerate() {
        let smudges = checks
            .into_iter()
            .map(|positions| pattern.count_smudges_between(positions))
            .sum::<usize>();
        if smudges == expected_smudges {
            return Some(center_minus_one + 1);
        }
    }
    None
}

#[test]
fn check_mirror_positions() {
    let str1 = "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.";
    let pattern1 = parse_data(str1);
    let str2 = "#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#";
    let pattern2 = parse_data(str2);
    assert_eq!(mirror_position(HorizontalPattern(&pattern1), 0), None);
    assert_eq!(mirror_position(HorizontalPattern(&pattern2), 0), Some(4));
    assert_eq!(mirror_position(VerticalPattern(&pattern1), 0), Some(5));
    assert_eq!(mirror_position(VerticalPattern(&pattern2), 0), None);
    assert_eq!(mirror_position(HorizontalPattern(&pattern1), 1), Some(3));
    assert_eq!(mirror_position(HorizontalPattern(&pattern2), 1), Some(1));
}

fn mirror_positions_sum<'a, P: Pattern + From<&'a Vec<&'a str>>>(
    data: &'a Vec<Vec<&str>>,
    expected_smudges: usize,
) -> usize {
    data.iter()
        .map(|pattern| mirror_position(P::from(pattern), expected_smudges))
        .map(Option::unwrap_or_default)
        .sum::<usize>()
}

fn combined_mirror_positions_sum<'a>(data: &Vec<Vec<&str>>, expected_smudges: usize) -> usize {
    let horizontal_mirror_sum = mirror_positions_sum::<HorizontalPattern>(data, expected_smudges);
    let vertical_mirror_sum = mirror_positions_sum::<VerticalPattern>(data, expected_smudges);
    vertical_mirror_sum + 100 * horizontal_mirror_sum
}

fn main() -> () {
    let file = std::fs::read_to_string("day13/input.txt").unwrap();

    let data = file.split("\n\n").map(parse_data).collect::<Vec<Vec<_>>>();

    let part1 = combined_mirror_positions_sum(&data, 0);
    println!("Part 1: {:?}", part1);

    let part2 = combined_mirror_positions_sum(&data, 1);
    println!("Part 2: {:?}", part2);
}
