use std::collections::HashSet;

type Point = (i32, i32, i32);

fn neighbors(&(x, y, z): &Point) -> [Point; 6] {
    [
        (x + 1, y, z),
        (x - 1, y, z),
        (x, y + 1, z),
        (x, y - 1, z),
        (x, y, z + 1),
        (x, y, z - 1),
    ]
}

fn parse_input(input: &str) -> Vec<Point> {
    input
        .trim()
        .lines()
        .map(|line| {
            let parts = line.split(",");
            let x = parts.map(|s| s.parse().unwrap()).collect::<Vec<_>>();
            if let [a, b, c] = x[..] {
                (a, b, c)
            } else {
                panic!("Oy!")
            }
        })
        .collect()
}

fn count_uncovered_sides(input: &Vec<Point>) -> usize {
    input
        .iter()
        .map(|p| {
            neighbors(p)
                .into_iter()
                .filter(|c| !input.contains(c))
                .count()
        })
        .sum()
}

fn part1(input: &Vec<Point>) -> usize {
    count_uncovered_sides(input)
}

#[test]
fn check_part1() {
    assert_eq!(part1(&vec![(1, 1, 1), (2, 1, 1)]), 10);
    assert_eq!(
        part1(&parse_input(
            &std::fs::read_to_string("day18/example.txt").unwrap()
        )),
        64
    );
}

fn part2(input: &Vec<Point>) -> usize {
    let mut airs: HashSet<Point> = HashSet::new();
    let rocks: HashSet<&Point> = HashSet::from_iter(input.iter());

    let min_x = input.iter().map(|p| p.0).min().unwrap() - 1;
    let max_x = input.iter().map(|p| p.0).max().unwrap() + 1;
    let min_y = input.iter().map(|p| p.1).min().unwrap() - 1;
    let max_y = input.iter().map(|p| p.1).max().unwrap() + 1;
    let min_z = input.iter().map(|p| p.2).min().unwrap() - 1;
    let max_z = input.iter().map(|p| p.2).max().unwrap() + 1;

    let mut todo = vec![(min_x, min_y, min_z)];

    while let Some(p) = todo.pop() {
        if !airs.contains(&p) && !rocks.contains(&p) {
            let (x, y, z) = p;
            if x >= min_x && x <= max_x && y >= min_y && y <= max_y && z >= min_z && z <= max_z {
                airs.insert(p);
                todo.extend(neighbors(&p).iter());
            }
        }
    }

    let outsides = (1 + max_x - min_x) * (1 + max_y - min_y)
        + (1 + max_x - min_x) * (1 + max_z - min_z)
        + (1 + max_y - min_y) * (1 + max_z - min_z);
    let uncovered = count_uncovered_sides(&airs.into_iter().collect());
    uncovered - 2 * usize::try_from(outsides).unwrap()
}

#[test]
fn check_part2() {
    assert_eq!(
        part2(&parse_input(
            &std::fs::read_to_string("day18/example.txt").unwrap()
        )),
        58
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day18/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    let part2 = part2(&parsed_input);
    println!("part 2: {}", part2);

    Ok(())
}
