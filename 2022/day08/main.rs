fn parse_input(input: &str) -> Vec<Vec<u32>> {
    input
        .trim()
        .lines()
        .map(|line| line.chars().map(|d| d.to_digit(10).unwrap()).collect())
        .collect()
}

fn part1(trees: &Vec<Vec<u32>>) -> usize {
    let mut checks: Vec<Vec<(usize, usize)>> = vec![];

    let rows = trees[0].len();
    let cols = trees.len();
    checks.extend((0..rows).map(|r| (0..cols).map(move |c| (r, c)).collect()));
    checks.extend((0..rows).map(|r| (0..cols).rev().map(move |c| (r, c)).collect()));
    checks.extend((0..cols).map(|c| (0..rows).map(move |r| (r, c)).collect()));
    checks.extend((0..cols).map(|c| (0..rows).rev().map(move |r| (r, c)).collect()));

    let mut visible = trees
        .iter()
        .map(|row| row.iter().map(|_| false).collect::<Vec<_>>())
        .collect::<Vec<_>>();

    for check in checks {
        let mut highest_so_far = None;
        for (row_index, col_index) in check {
            let tree = trees[row_index][col_index];
            if highest_so_far.and_then(|x| (tree <= x).then_some(0)) == None {
                highest_so_far = Some(tree);
                visible[row_index][col_index] = true;
            }
        }
    }

    visible
        .iter()
        .map(|row| row.iter().filter(|&&r| r).count())
        .sum()
}

#[test]
fn check_part1() {
    assert_eq!(
        part1(&parse_input(
            &std::fs::read_to_string("day08/example.txt").unwrap()
        )),
        21
    );
}

fn part2(trees: &Vec<Vec<u32>>) -> usize {
    let mut scenic_scores = trees
        .iter()
        .map(|row| row.iter().map(|_| 0).collect::<Vec<usize>>())
        .collect::<Vec<_>>();

    for (outer_row_index, outer_row) in trees.iter().enumerate() {
        for (outer_col_index, &outer_tree) in outer_row.iter().enumerate() {
            let mut outer_scenic_score = 1;

            let checks = [
                trees[outer_row_index][(outer_col_index + 1)..].to_vec(),
                trees[outer_row_index][..outer_col_index]
                    .iter()
                    .rev()
                    .copied()
                    .collect(),
                trees[(outer_row_index + 1)..]
                    .iter()
                    .map(|row| row[outer_col_index])
                    .collect(),
                trees[..outer_row_index]
                    .iter()
                    .rev()
                    .map(|row| row[outer_col_index])
                    .collect(),
            ];

            for check in checks {
                let mut visible = 0;
                for tree in check {
                    visible += 1;
                    if tree >= outer_tree {
                        break;
                    }
                }
                outer_scenic_score *= visible;
            }

            scenic_scores[outer_row_index][outer_col_index] = outer_scenic_score;
        }
    }

    scenic_scores
        .into_iter()
        .map(|row| row.into_iter().max().unwrap())
        .max()
        .unwrap()
}

#[test]
fn check_part2() {
    assert_eq!(
        part2(&parse_input(
            &std::fs::read_to_string("day08/example.txt").unwrap()
        )),
        8
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day08/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    let part2 = part2(&parsed_input);
    println!("part 2: {}", part2);

    Ok(())
}
