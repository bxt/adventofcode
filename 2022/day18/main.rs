fn parse_input(input: &str) -> Vec<(i32, i32, i32)> {
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

fn part1(input: &Vec<(i32, i32, i32)>) -> usize {
    input.len();

    input
        .iter()
        .map(|&(x, y, z)| {
            [
                (x + 1, y, z),
                (x - 1, y, z),
                (x, y + 1, z),
                (x, y - 1, z),
                (x, y, z + 1),
                (x, y, z - 1),
            ]
            .into_iter()
            .filter(|c| !input.contains(c))
            .count()
        })
        .sum()
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day18/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    // let part2 = part2(&parsed_input);
    // println!("part 2: {}", part2);

    Ok(())
}
