fn transpose_lines(lines: Vec<&str>) -> Vec<String> {
    (0..(lines[0].len()))
        .map(|index| {
            String::from_utf8(
                lines
                    .iter()
                    .map(|line| line.bytes().nth(index).unwrap())
                    .collect(),
            )
            .unwrap()
        })
        .collect()
}

fn weight_after_shift(input: &str) -> usize {
    let mut total_weight = 0;
    let mut next_weight = input.len();
    for (index, byte) in input.bytes().enumerate() {
        match byte {
            b'O' => {
                total_weight += next_weight;
                next_weight -= 1;
            }
            b'#' => {
                next_weight = input.len() - index - 1;
            }
            b'.' => {}
            _ => {
                panic!("Not sure what to do with {byte}")
            }
        }
    }
    total_weight
}

#[test]
fn check_weight_after_shift() {
    assert_eq!(weight_after_shift("O...."), 5);
    assert_eq!(weight_after_shift(".O..."), 5);
    assert_eq!(weight_after_shift("OO..."), 9);
    assert_eq!(weight_after_shift("...OO"), 9);
    assert_eq!(weight_after_shift("..O#."), 5);
    assert_eq!(weight_after_shift("..#O."), 2);
    assert_eq!(weight_after_shift("..#OO"), 3);
    assert_eq!(weight_after_shift("###OO"), 3);
    assert_eq!(weight_after_shift(".#OO."), 5);
    assert_eq!(weight_after_shift(".O#OO"), 8);
}

fn main() -> () {
    let file = std::fs::read_to_string("day14/input.txt").unwrap();

    let lines = file
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .collect::<Vec<_>>();

    let lines_transposed = transpose_lines(lines);

    let part1 = lines_transposed
        .iter()
        .map(|line| weight_after_shift(&line))
        .sum::<usize>();

    println!("Part 1: {:?}", part1);
}
