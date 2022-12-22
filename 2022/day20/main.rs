use std::cmp::{max, min};

fn parse_input(input: &str) -> Vec<i32> {
    input.trim().lines().map(|l| l.parse().unwrap()).collect()
}

fn part1(input: &Vec<i32>) -> i32 {
    dbg!(input);
    dbg!(input.len());
    let len = input.len();
    let len_i32 = i32::try_from(input.len()).unwrap();
    let mut shuffled = vec![];
    input.clone_into(&mut shuffled);

    for number in input {
        let from = shuffled.iter().position(|n| n == number).unwrap();
        let from_i32 = i32::try_from(from).unwrap();
        let to = ((from_i32 + number) % len_i32 + len_i32) % len_i32;
        let to_usize = usize::try_from(to).unwrap();
        dbg!(from);
        dbg!(to);
        // if to_usize < from {
        // shuffled.remove(from);
        // shuffled.insert(to_usize, *number);
        // } else {
        //     shuffled.remove(from);
        //     shuffled.insert(to_usize, *number);
        // }
        let begin = min(from, to_usize);
        let end = max(from, to_usize);

        for index in begin..end {
            let tmp = shuffled[index];
            shuffled[index] = shuffled[index + 1];
            shuffled[index + 1] = tmp;
        }

        dbg!(&shuffled);
    }

    let results = [1, 2, 3].map(|n| shuffled[(n * 1000) % len]);
    results.into_iter().sum()
}

#[test]
fn check_part1() {
    assert_eq!(part1(&parse_input("1\n2\n-3\n3\n-2\n0\n4")), 3);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day20/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    // let part2 = part2(&parsed_input);
    // println!("part 2: {}", part2);

    Ok(())
}
