#[derive(Clone)]
struct TakeWhileInclusive<I, P> {
    iter: I,
    flag: bool,
    predicate: P,
}

impl<I, P> TakeWhileInclusive<I, P> {
    fn new(iter: I, predicate: P) -> TakeWhileInclusive<I, P> {
        TakeWhileInclusive {
            iter,
            flag: false,
            predicate,
        }
    }
}

impl<I: Iterator, P> Iterator for TakeWhileInclusive<I, P>
where
    P: FnMut(&I::Item) -> bool,
{
    type Item = I::Item;

    #[inline]
    fn next(&mut self) -> Option<I::Item> {
        if self.flag {
            None
        } else {
            let x = self.iter.next()?;
            if (self.predicate)(&x) {
                Some(x)
            } else {
                self.flag = true;
                Some(x)
            }
        }
    }
}

#[test]
fn check_true_once_more_take_while() {
    let iter = [1, 3, 4, 6, 9].iter();
    let predicate = |&&x| x < 5;
    TakeWhileInclusive::new(iter, predicate).next();
    let result: Vec<i32> = TakeWhileInclusive::new(iter, predicate).collect();
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
