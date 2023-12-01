fn parse_line(line: &str) -> u32 {
    let pieces = line.trim().split("");
    let numbers = pieces
        .filter_map(|letter| letter.parse::<u32>().ok())
        .collect::<Vec<u32>>();
    // println!("numbers: {:?}", numbers);
    numbers[0] * 10 + numbers[numbers.len() - 1]
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day01/input.txt")?;

    let part1: u32 = file
        .split("\n")
        .filter(|line| !line.is_empty())
        .map(|line| parse_line(line))
        .sum();

    println!("part 1: {}", part1);

    Ok(())
}
