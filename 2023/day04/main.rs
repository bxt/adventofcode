fn parse_number_list(input: &str) -> Vec<u32> {
    input
        .split(" ")
        .filter(|s| !s.is_empty())
        .map(|s| s.parse().unwrap())
        .collect()
}

fn card_win_count((winning, have): (Vec<u32>, Vec<u32>)) -> usize {
    have.iter().filter(|n| winning.contains(n)).count()
}

fn card_score(&count: &usize) -> usize {
    if count == 0 {
        count
    } else {
        1 << count - 1
    }
}

#[test]
fn check_card_score() {
    assert_eq!(card_score(&card_win_count((vec![], vec![]))), 0);
    assert_eq!(card_score(&card_win_count((vec![1], vec![1]))), 1);
    assert_eq!(card_score(&card_win_count((vec![1, 2], vec![1, 2]))), 2);
    assert_eq!(
        card_score(&card_win_count((vec![1, 2, 3], vec![1, 2, 3]))),
        4
    );
    assert_eq!(
        card_score(&card_win_count((vec![1, 2, 3], vec![1, 2, 9]))),
        2
    );
}

fn pile_up_cards(card_win_counts: Vec<usize>) -> usize {
    let mut card_amounts = vec![1; card_win_counts.len()];
    for index in 0..card_win_counts.len() {
        for offset in 0..card_win_counts[index] {
            card_amounts[index + offset + 1] += card_amounts[index];
        }
    }
    card_amounts.into_iter().sum::<usize>()
}

#[test]
fn check_pile_up_cards() {
    assert_eq!(pile_up_cards(vec![]), 0);
    assert_eq!(pile_up_cards(vec![0]), 1);
    assert_eq!(pile_up_cards(vec![0, 0]), 2);
    assert_eq!(pile_up_cards(vec![4, 2, 2, 1, 0, 0]), 30);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day04/input.txt")?;

    let wins = file
        .split("\n")
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .map(|line| {
            let (winning_str, have_str) = line.split_once(":").unwrap().1.split_once("|").unwrap();
            (parse_number_list(winning_str), parse_number_list(have_str))
        })
        .map(card_win_count)
        .collect::<Vec<_>>();

    println!("Part 1: {:?}", wins.iter().map(card_score).sum::<usize>());
    println!("Part 2: {:?}", pile_up_cards(wins));

    Ok(())
}
