fn true_once_more<A, F>(predicate: F) -> impl FnMut(A) -> bool
where
    F: Fn(A) -> bool,
{
    let mut was_false_once = false;
    move |input| {
        if was_false_once {
            return false;
        }
        let result = predicate(input);
        if !result {
            was_false_once = true;
            return true;
        }
        result
    }
}

#[test]
fn check_true_once_more_simple() {
    let mut closure = true_once_more(|x| x < 5);
    assert_eq!(closure(3), true);
    assert_eq!(closure(1), true);
    assert_eq!(closure(5), true);
    assert_eq!(closure(1), false);
}

#[test]
fn check_true_once_more_take_while() {
    let mut closure = true_once_more(|&&x| x < 5);
    let mut closure2: impl FnMut<&i23> -> bool = |&x| x < 5;

    let iter = [1, 3, 4, 6, 9].iter().take_while(closure2);
    let foo = iter.next();
    let result: Vec<i32> = iter.collect();
    assert_eq!(vec![1, 2, 3, 6], result);
}

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
    println!("trees: {:?}", trees);

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

    println!("visible: {:?}", visible);

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
    println!("trees: {:?}", trees);

    let mut scenic_scores = trees
        .iter()
        .map(|row| row.iter().map(|_| 0).collect::<Vec<usize>>())
        .collect::<Vec<_>>();

    for (outer_row_index, outer_row) in trees.iter().enumerate() {
        for (outer_col_index, outer_tree) in outer_row.iter().enumerate() {
            let mut outer_scenic_score = 1;

            {
                let visible = trees[outer_row_index][(outer_col_index + 1)..]
                    .iter()
                    .count();
                outer_scenic_score *= visible;
            }

            println!(
                "score at {} {}: {:?}",
                outer_row_index, outer_col_index, outer_scenic_score
            );
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
