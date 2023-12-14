type Data<'a> = Vec<&'a str>;

fn parse_data(input: &str) -> Data {
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
    fn smudges_match_expectation(&self, smudges: usize) -> bool;

    fn mirror_position(&self) -> Option<usize> {
        for (center_minus_one, checks) in potential_mirroring(self.len()).enumerate() {
            let smudges = checks
                .into_iter()
                .map(|positions| self.count_smudges_between(positions))
                .sum::<usize>();
            if self.smudges_match_expectation(smudges) {
                return Some(center_minus_one + 1);
            }
        }
        None
    }
}

struct HorizontalPattern<'a, const SMUDGES: usize>(&'a Data<'a>);

impl<'a, const S: usize> From<&'a Data<'a>> for HorizontalPattern<'a, S> {
    fn from(value: &'a Data) -> Self {
        HorizontalPattern(value)
    }
}

impl<'a, const SMUDGES: usize> Pattern for HorizontalPattern<'_, SMUDGES> {
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

    fn smudges_match_expectation(&self, smudges: usize) -> bool {
        smudges == SMUDGES
    }
}

struct VerticalPattern<'a, const SMUDGES: usize>(&'a Data<'a>);

impl<'a, const S: usize> From<&'a Data<'a>> for VerticalPattern<'a, S> {
    fn from(value: &'a Data) -> Self {
        VerticalPattern(value)
    }
}

impl<'a, const SMUDGES: usize> Pattern for VerticalPattern<'_, SMUDGES> {
    fn len(&self) -> usize {
        self.0[0].len()
    }

    fn count_smudges_between(&self, (a, b): (usize, usize)) -> usize {
        self.0
            .iter()
            .filter(|line| line.bytes().nth(a) != line.bytes().nth(b))
            .count()
    }

    fn smudges_match_expectation(&self, smudges: usize) -> bool {
        smudges == SMUDGES
    }
}

#[test]
fn check_mirror_positions() {
    let str1 = "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.";
    let pattern1 = parse_data(str1);
    let str2 = "#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#";
    let pattern2 = parse_data(str2);
    assert_eq!(HorizontalPattern::<0>(&pattern1).mirror_position(), None);
    assert_eq!(HorizontalPattern::<0>(&pattern2).mirror_position(), Some(4));
    assert_eq!(VerticalPattern::<0>(&pattern1).mirror_position(), Some(5));
    assert_eq!(VerticalPattern::<0>(&pattern2).mirror_position(), None);
    assert_eq!(HorizontalPattern::<1>(&pattern1).mirror_position(), Some(3));
    assert_eq!(HorizontalPattern::<1>(&pattern2).mirror_position(), Some(1));
}

fn mirror_positions_sum<'a, P: Pattern + From<&'a Data<'a>>>(data: &'a Vec<Data>) -> usize {
    data.iter()
        .map(|pattern| P::from(pattern).mirror_position())
        .map(Option::unwrap_or_default)
        .sum::<usize>()
}

fn combined_mirror_positions_sum<'a, const SMUDGES: usize>(data: &Vec<Data>) -> usize {
    let horizontal_mirror_sum = mirror_positions_sum::<HorizontalPattern<SMUDGES>>(data);
    let vertical_mirror_sum = mirror_positions_sum::<VerticalPattern<SMUDGES>>(data);
    vertical_mirror_sum + 100 * horizontal_mirror_sum
}

fn main() -> () {
    let file = std::fs::read_to_string("day13/input.txt").unwrap();
    let data = file.split("\n\n").map(parse_data).collect::<Vec<Vec<_>>>();
    println!("Part 1: {:?}", combined_mirror_positions_sum::<0>(&data));
    println!("Part 2: {:?}", combined_mirror_positions_sum::<1>(&data));
}
