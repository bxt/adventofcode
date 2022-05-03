use std::cmp;

fn parse_line(line: &str) -> Result<(u32, u32), Box<dyn std::error::Error>> {
    let mut pieces = line.split("-");
    let from_string = pieces.next().ok_or("no from")?;
    let to_string = pieces.next().ok_or("no to")?;
    let disallow_from: u32 = from_string.parse()?;
    let disallow_to: u32 = to_string.parse()?;
    Ok((disallow_from, disallow_to))
}

fn disallow(allowed: Vec<(u32, u32)>, (disallow_from, disallow_to): (u32, u32)) -> Vec<(u32, u32)> {
    let mut new_allowed = vec![];

    for (allowed_from, allowed_to) in allowed {
        if disallow_from <= allowed_from {
            if disallow_to < allowed_to {
                new_allowed.push((cmp::max(disallow_to + 1, allowed_from), allowed_to))
            }
        } else {
            new_allowed.push((allowed_from, cmp::min(disallow_from - 1, allowed_to)));
            if disallow_to < allowed_to {
                new_allowed.push((disallow_to + 1, allowed_to));
            }
        }
    }

    new_allowed
}

#[test]
fn chec_disallow() {
    assert_eq!(disallow(vec![(5, 7)], (2, 4)), vec![(5, 7)]);
    assert_eq!(disallow(vec![(5, 7)], (5, 7)), vec![]);
    assert_eq!(disallow(vec![(5, 7)], (2, 7)), vec![]);
    assert_eq!(disallow(vec![(5, 7)], (2, 6)), vec![(7, 7)]);
    assert_eq!(disallow(vec![(5, 7)], (5, 5)), vec![(6, 7)]);
    assert_eq!(disallow(vec![(5, 7)], (6, 6)), vec![(5, 5), (7, 7)]);
    assert_eq!(disallow(vec![(5, 7)], (6, 8)), vec![(5, 5)]);
    assert_eq!(disallow(vec![(5, 7)], (7, 8)), vec![(5, 6)]);
    assert_eq!(disallow(vec![(5, 7)], (8, 9)), vec![(5, 7)]);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("input.txt")?;

    let allowed = file
        .lines()
        .map(|l| parse_line(l).unwrap())
        .fold(vec![(0u32, u32::MAX)], disallow);

    println!("part 1: {}", allowed[0].0);

    let mut sum = 0u32;
    for (allowed_from, allowed_to) in allowed {
        sum += allowed_to - allowed_from + 1;
    }

    println!("part 2: {}", sum);

    Ok(())
}
