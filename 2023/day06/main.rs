fn parse_number_list(input: &str) -> Vec<u64> {
    input
        .split(" ")
        .filter(|s| !s.is_empty())
        .map(|s| s.parse().unwrap())
        .collect()
}

fn count_win_options((time, distance): (u64, u64)) -> u64 {
    let mut count = 0;
    for button_time in 0..time {
        let speed = button_time;
        let drive_time = time - button_time;
        let reached_distance = drive_time * speed;
        if reached_distance > distance {
            count += 1;
        }
    }
    count
}

#[test]
fn check_count_win_options() {
    assert_eq!(count_win_options((7, 9)), 4);
    assert_eq!(count_win_options((15, 40)), 8);
    assert_eq!(count_win_options((30, 200)), 9);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day06/input.txt")?;
    let mut lines = file.split("\n");
    let times = parse_number_list(lines.next().unwrap().strip_prefix("Time:").unwrap());
    let distances = parse_number_list(lines.next().unwrap().strip_prefix("Distance:").unwrap());
    let input = times.into_iter().zip(distances).collect::<Vec<_>>();

    let part1 = input.into_iter().map(count_win_options).product::<u64>();
    println!("Part 1: {:?}", part1);

    Ok(())
}
