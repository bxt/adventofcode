fn parse_input(input: &str) -> Vec<Vec<u32>> {
    input
        .trim()
        .lines()
        .map(|line| {
            line.chars()
                .map(|d| d.to_digit(10).unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
}

fn part1(trees: &Vec<Vec<u32>>) -> usize {
    let mut visible = trees
        .iter()
        .map(|row| row.iter().map(|_| false).collect::<Vec<_>>())
        .collect::<Vec<_>>();

    for (row_index, row) in trees.iter().enumerate() {
        let mut highest_so_far = None;
        for (col_index, tree) in row.iter().enumerate() {
            if highest_so_far.and_then(|x| (tree <= x).then_some(0)) == None {
                highest_so_far = Some(tree);
                visible[row_index][col_index] = true;
            }
        }
    }

    for (row_index, row) in trees.iter().enumerate() {
        let mut highest_so_far = None;
        for (col_index, tree) in row.iter().enumerate().rev() {
            if highest_so_far.and_then(|x| (tree <= x).then_some(0)) == None {
                highest_so_far = Some(tree);
                visible[row_index][col_index] = true;
            }
        }
    }

    for (col_index, _) in trees[0].iter().enumerate() {
        let mut highest_so_far = None;
        for (row_index, row) in trees.iter().enumerate() {
            let tree = row[col_index];
            if highest_so_far.and_then(|x| (tree <= x).then_some(0)) == None {
                highest_so_far = Some(tree);
                visible[row_index][col_index] = true;
            }
        }
    }

    for (col_index, _) in trees[0].iter().enumerate() {
        let mut highest_so_far = None;
        for (row_index, row) in trees.iter().enumerate().rev() {
            let tree = row[col_index];
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
        for (outer_col_index, outer_tree) in outer_row.iter().enumerate() {
            let mut outer_scenic_score = 1;

            {
                let mut visible = 0;
                for tree in &trees[outer_row_index][(outer_col_index + 1)..] {
                    visible += 1;
                    if tree >= outer_tree {
                        break;
                    }
                }
                outer_scenic_score *= visible;
            }

            {
                let mut visible = 0;
                for tree in trees[outer_row_index][..outer_col_index].iter().rev() {
                    visible += 1;
                    if tree >= outer_tree {
                        break;
                    }
                }
                outer_scenic_score *= visible;
            }

            {
                let mut visible = 0;
                for row in trees[(outer_row_index + 1)..].iter() {
                    let tree = &row[outer_col_index];
                    visible += 1;
                    if tree >= outer_tree {
                        break;
                    }
                }
                outer_scenic_score *= visible;
            }

            {
                let mut visible = 0;
                for row in trees[..outer_row_index].iter().rev() {
                    let tree = &row[outer_col_index];
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
