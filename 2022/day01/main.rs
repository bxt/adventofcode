
fn parse_block(block: &str) -> Vec<u32> {
    let pieces = block.trim().split("\n");
    pieces.map(|line| line.parse::<u32>().unwrap()).collect()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day01/input.txt")?;

    let parsed_input  = file
        .split("\n\n")
        .map(|block| parse_block(block)).collect::<Vec<Vec<u32>>>();

    let mut sums = parsed_input.iter().map(|block| block.iter().sum()).collect::<Vec<u32>>();

    let part1 = sums.iter().max().unwrap();

    println!("part 1: {}", part1);

    sums.sort();

    let part2 = sums[sums.len()-3..sums.len()].iter().sum::<u32>();

    println!("part 2: {}", part2);

    Ok(())
}
