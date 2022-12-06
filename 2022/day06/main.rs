fn part1(input: &str) -> u32 {
    let mut index = 0;
    let mut c1 = None;
    let mut c2 = None;
    let mut c3 = None;
    let mut c4 = None;

    for c in input.chars() {
        if index >= 4 && c1 != c2 && c1 != c3 && c1 != c4 && c2 != c3 && c2 != c4 && c3 != c4 {
            return index;
        }

        (c1, c2, c3, c4) = (c2, c3, c4, Some(c));

        index += 1;
    }

    0
}

#[test]
fn check_part1() {
    assert_eq!(part1("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 7);
    assert_eq!(part1("bvwbjplbgvbhsrlpgdmjqwftvncz"), 5);
    assert_eq!(part1("nppdvjthqldpwncqszvftbrmjlhg"), 6);
    assert_eq!(part1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 10);
    assert_eq!(part1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 11);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day06/input.txt")?;

    let part1 = part1(&file);
    println!("part 1: {}", part1);

    // let part2 = part2(parsed_input2);
    // println!("part 2: {}", part2);

    Ok(())
}
