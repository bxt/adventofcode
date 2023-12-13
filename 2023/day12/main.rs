use std::{collections::HashMap, iter::once};

fn parse_springs(input: &str) -> Vec<(&str, Vec<usize>)> {
    input
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .map(|l| {
            let (mask, lengths_str) = l.split_once(" ").unwrap();
            let lengths = lengths_str.split(",").map(|n| n.parse().unwrap()).collect();
            (mask, lengths)
        })
        .collect()
}

fn push_option(options: &mut HashMap<(usize, usize), usize>, state: (usize, usize), count: usize) {
    *options.entry(state).or_insert(0) += count;
}

fn eat((mask_index, length_index): (usize, usize)) -> (usize, usize) {
    (mask_index + 1, length_index)
}

fn jump((_, length_index): (usize, usize)) -> (usize, usize) {
    (0, length_index + 1)
}

fn count_possibilities(record: &(&str, Vec<usize>)) -> usize {
    let (mask, lengths) = record;

    let mut options = HashMap::from([((0, 0), 1)]);

    for mask_byte in mask.bytes().chain(once(b'.')) {
        let mut new_options = HashMap::new();
        for (state, count) in options {
            let (mask_index, length_index) = state;
            if mask_byte == b'#' || mask_byte == b'?' {
                if length_index < lengths.len() && mask_index < lengths[length_index] {
                    push_option(&mut new_options, eat(state), count);
                } // else we ate too much, discard option
            }
            if mask_byte == b'.' || mask_byte == b'?' {
                if mask_index == 0 {
                    push_option(&mut new_options, state, count);
                } else if length_index < lengths.len() && mask_index == lengths[length_index] {
                    push_option(&mut new_options, jump(state), count);
                } // else we found a space in the middle of the length, discard option
            }
        }
        options = new_options;
    }

    *options.get(&(0, lengths.len())).unwrap_or(&0)
}

#[test]
fn check_count_possibilities() {
    assert_eq!(count_possibilities(&("???.###", vec![1, 1, 3])), 1);
    assert_eq!(count_possibilities(&(".??..??...?##.", vec![1, 1, 3])), 4);
    let mask3 = "?#?#?#?#?#?#?#?";
    assert_eq!(count_possibilities(&(mask3, vec![1, 3, 1, 6])), 1);
    assert_eq!(count_possibilities(&("????.#...#...", vec![4, 1, 1])), 1);
    let mask5 = "????.######..#####.";
    assert_eq!(count_possibilities(&(mask5, vec![1, 6, 5])), 4);
    assert_eq!(count_possibilities(&("?###????????", vec![3, 2, 1])), 10);
}

fn unfold(record: &(&str, Vec<usize>)) -> (String, Vec<usize>) {
    let (mask, lengths) = record;
    let new_mask = format!("{}?{0}?{0}?{0}?{0}", mask);
    let new_lengths = lengths.repeat(5);
    (new_mask, new_lengths)
}

fn count_unfolded_possibilities(record: &(&str, Vec<usize>)) -> usize {
    let (mask, lengths) = unfold(record);
    count_possibilities(&(&mask, lengths))
}

#[test]
fn check_count_unfolded_possibilities() {
    assert_eq!(count_unfolded_possibilities(&("???.###", vec![1, 1, 3])), 1);
    let mask2 = ".??..??...?##.";
    assert_eq!(count_unfolded_possibilities(&(mask2, vec![1, 1, 3])), 16384);
    let mask3 = "?#?#?#?#?#?#?#?";
    assert_eq!(count_unfolded_possibilities(&(mask3, vec![1, 3, 1, 6])), 1);
    let mask4 = "????.#...#...";
    assert_eq!(count_unfolded_possibilities(&(mask4, vec![4, 1, 1])), 16);
    let mask5 = "????.######..#####.";
    assert_eq!(count_unfolded_possibilities(&(mask5, vec![1, 6, 5])), 2500);
    let mask6 = "?###????????";
    let r6 = 506250;
    assert_eq!(count_unfolded_possibilities(&(mask6, vec![3, 2, 1])), r6);
}

fn main() -> () {
    let file = std::fs::read_to_string("day12/input.txt").unwrap();

    let data = parse_springs(&file);

    let part1 = data.iter().map(count_possibilities).sum::<usize>();
    println!("Part 1: {:?}", part1);

    let part2 = data.iter().map(count_unfolded_possibilities).sum::<usize>();
    println!("Part 2: {:?}", part2);
}
