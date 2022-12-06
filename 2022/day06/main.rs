use std::collections::HashSet;

fn find_marker(input: &str, window_size: usize) -> usize {
    let vec = input.chars().collect::<Vec<char>>();

    vec.windows(window_size)
        .position(|window| window.iter().collect::<HashSet<_>>().len() == window_size)
        .unwrap()
        + window_size
}

#[test]
fn check_part1() {
    assert_eq!(find_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4), 7);
    assert_eq!(find_marker("bvwbjplbgvbhsrlpgdmjqwftvncz", 4), 5);
    assert_eq!(find_marker("nppdvjthqldpwncqszvftbrmjlhg", 4), 6);
    assert_eq!(find_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4), 10);
    assert_eq!(find_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4), 11);
}

#[test]
fn check_part2() {
    assert_eq!(find_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14), 19);
    assert_eq!(find_marker("bvwbjplbgvbhsrlpgdmjqwftvncz", 14), 23);
    assert_eq!(find_marker("nppdvjthqldpwncqszvftbrmjlhg", 14), 23);
    assert_eq!(find_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14), 29);
    assert_eq!(find_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14), 26);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day06/input.txt")?;

    let part1 = find_marker(&file, 4);
    println!("part 1: {}", part1);

    let part2 = find_marker(&file, 14);
    println!("part 2: {}", part2);

    Ok(())
}
