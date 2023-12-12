use std::vec;

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

fn count_possibilities(record: &(&str, Vec<usize>)) -> usize {
    dbg!(record);
    let (mask, lengths) = record;
    let total_space = mask.len() - lengths.iter().sum::<usize>();
    let mut possibilities = vec![vec![]];
    for index in 0..=lengths.len() {
        possibilities = possibilities
            .iter()
            .flat_map(|spaces: &Vec<usize>| {
                let space_left = total_space - spaces.iter().sum::<usize>();
                let is_last_space = index == lengths.len();
                let range = if is_last_space {
                    space_left..=space_left
                } else {
                    let reserved_for_rest = match index {
                        0 => lengths.len() - 1,
                        _ if is_last_space => 0,
                        _ => lengths.len() - index - 1,
                    };
                    let min_size = match index {
                        0 => 0,
                        _ if is_last_space => 0,
                        _ => 1,
                    };
                    let max_size = space_left - reserved_for_rest;
                    min_size..=max_size
                };
                // dbg!(spaces);
                // dbg!(space_left);
                // dbg!(is_last_space);
                // dbg!(reserved_for_rest);
                // dbg!(min_size);
                range
                    .map(|space| {
                        let mut new_spaces = vec![];
                        new_spaces.extend(spaces);
                        new_spaces.push(space);
                        new_spaces
                    })
                    .collect::<Vec<_>>()
            })
            .collect();
    }
    dbg!(possibilities.len());
    // dbg!(&possibilities);

    let valid_possibilities = possibilities
        .iter()
        .filter(|spaces| {
            // [4, 1, 1]
            // if spaces.to_vec() != vec![0, 1, 3, 3] {
            //     return false;
            // }
            // dbg!("all space?");
            let spaces_are_valid = spaces.iter().enumerate().all(|(index, space)| {
                let space_before = spaces.iter().take(index).sum::<usize>();
                let hashes_before = lengths.iter().take(index).sum::<usize>();
                let start = space_before + hashes_before;
                // dbg!(index);
                // dbg!(space);
                // dbg!(space_before);
                // dbg!(hashes_before);
                // dbg!(start);
                (0..*space).all(|offset| {
                    let index = start + offset;
                    let byte = mask.as_bytes()[index];
                    // dbg!((index, byte));
                    byte == b'?' || byte == b'.'
                })
            });

            // dbg!("all occ?");

            let hashes_are_valid = lengths.iter().enumerate().all(|(index, length)| {
                let space_before = spaces.iter().take(index + 1).sum::<usize>();
                let hashes_before = lengths.iter().take(index).sum::<usize>();
                let start = space_before + hashes_before;
                (0..*length).all(|offset| {
                    let index = start + offset;
                    let byte = mask.as_bytes()[index];
                    // dbg!((index, byte));
                    byte == b'?' || byte == b'#'
                })
            });

            spaces_are_valid && hashes_are_valid
        })
        .collect::<Vec<_>>();

    // dbg!(&valid_possibilities);

    valid_possibilities.len()
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

fn main() -> () {
    let file = std::fs::read_to_string("day12/input.txt").unwrap();

    let data = parse_springs(&file);
    let part1 = data.iter().map(count_possibilities).sum::<usize>();
    println!("Part 1: {:?}", part1);
}
