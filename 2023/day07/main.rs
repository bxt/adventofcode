use std::collections::HashMap;
use std::hash::Hash;

const JOKER: u8 = 11;

fn parse_hand(input: &str) -> Vec<u8> {
    input
        .bytes()
        .map(|b| match b {
            b'2'..=b'9' => b - b'0',
            b'A' => 14,
            b'K' => 13,
            b'Q' => 12,
            b'J' => JOKER,
            b'T' => 10,
            _ => panic!("Not a card: {b}"),
        })
        .collect()
}

#[test]
fn check_parse_hand() {
    assert_eq!(parse_hand("123456789"), vec![1, 2, 3, 4, 5, 6, 7, 8, 9]);
    assert_eq!(parse_hand("AKQJT"), vec![14, 13, 12, 11, 10]);
}

fn parse_hands(input: &str) -> Vec<(Vec<u8>, i32)> {
    input
        .split("\n")
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .map(|l| {
            let (hand_str, bid_str) = l.split_once(" ").expect(&format!("no space in '{l}'"));
            (parse_hand(hand_str), bid_str.parse().unwrap())
        })
        .collect()
}

fn counts<T: Eq + Hash + Copy>(input: &[T]) -> HashMap<T, usize> {
    let mut result = HashMap::new();
    for &element in input {
        *result.entry(element).or_insert(0) += 1;
    }
    result
}

#[test]
fn check_counts() {
    assert_eq!(
        counts(&vec!['a', 'b', 'b', 'c', 'b', 'a']),
        HashMap::from([('a', 2), ('b', 3), ('c', 1),])
    );
}

fn tally_top_list(tally: HashMap<u8, usize>) -> Vec<usize> {
    let mut counts = tally.into_iter().map(|e| e.1).collect::<Vec<_>>();
    counts.sort();
    counts.reverse();
    counts
}

fn hand_rank_vector_p1(hand: &[u8]) -> Vec<usize> {
    tally_top_list(counts(hand))
        .into_iter()
        .chain(hand.iter().map(|e| usize::from(*e)))
        .collect()
}

#[test]
fn check_hand_rank_vector_p1() {
    assert_eq!(
        hand_rank_vector_p1(&parse_hand("32T3K")),
        vec![2, 1, 1, 1, 3, 2, 10, 3, 13]
    );
    assert_eq!(
        hand_rank_vector_p1(&parse_hand("KK677")),
        vec![2, 2, 1, 13, 13, 6, 7, 7]
    );
    assert_eq!(
        hand_rank_vector_p1(&parse_hand("KTJJT")),
        vec![2, 2, 1, 13, 10, 11, 11, 10]
    );
    assert_eq!(
        hand_rank_vector_p1(&parse_hand("T55J5")),
        vec![3, 1, 1, 10, 5, 5, 11, 5]
    );
}

fn hand_rank_vector_p2(hand: &[u8]) -> Vec<usize> {
    let mut tally = counts(hand);

    if let Some((_, joker_count)) = tally.remove_entry(&JOKER) {
        if let Some(use_for) = tally.values_mut().max() {
            *use_for += joker_count;
        } else {
            tally.insert(JOKER, joker_count);
        }
    }

    tally_top_list(tally)
        .into_iter()
        .chain(
            hand.iter()
                .map(|&e| if e == JOKER { 1 } else { usize::from(e) }),
        )
        .collect()
}

#[test]
fn check_hand_rank_vector_p2() {
    assert_eq!(
        hand_rank_vector_p2(&parse_hand("32T3K")),
        vec![2, 1, 1, 1, 3, 2, 10, 3, 13]
    );
    assert_eq!(
        hand_rank_vector_p2(&parse_hand("KK677")),
        vec![2, 2, 1, 13, 13, 6, 7, 7]
    );
    assert_eq!(
        hand_rank_vector_p2(&parse_hand("KTJJT")),
        vec![4, 1, 13, 10, 1, 1, 10]
    );
    assert_eq!(
        hand_rank_vector_p2(&parse_hand("T55J5")),
        vec![4, 1, 10, 5, 5, 1, 5]
    );
    assert_eq!(
        hand_rank_vector_p2(&parse_hand("JJJJJ")),
        vec![5, 1, 1, 1, 1, 1]
    );
}

fn winnings(input: &[(Vec<u8>, i32)], hand_rank_vector: &dyn Fn(&[u8]) -> Vec<usize>) -> i32 {
    let mut input_mut = input.to_vec();
    input_mut.sort_by_key(|(hand, _)| hand_rank_vector(hand));
    input_mut
        .iter()
        .enumerate()
        .map(|(rank, (_, bid))| (i32::try_from(rank).unwrap() + 1) * bid)
        .sum::<i32>()
}

#[test]
fn check_winnings() {
    let example = vec![
        (parse_hand("32T3K"), 765),
        (parse_hand("T55J5"), 684),
        (parse_hand("KK677"), 28),
        (parse_hand("KTJJT"), 220),
        (parse_hand("QQQJA"), 483),
    ];
    assert_eq!(winnings(&example, &hand_rank_vector_p1), 6440);
    assert_eq!(winnings(&example, &hand_rank_vector_p2), 5905);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day07/input.txt")?;
    let hands = parse_hands(&file);

    println!("Part 1: {:?}", winnings(&hands, &hand_rank_vector_p1));
    println!("Part 2: {:?}", winnings(&hands, &hand_rank_vector_p2));

    Ok(())
}
