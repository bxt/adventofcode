use std::{collections::HashMap, ops::Index};

#[derive(Debug, PartialEq)]
struct Crossroads<T>(T, T);

impl<T> Index<bool> for Crossroads<T> {
    type Output = T;

    fn index(&self, index: bool) -> &Self::Output {
        if index {
            &self.1
        } else {
            &self.0
        }
    }
}

fn parse_instructions(input: &str) -> Vec<bool> {
    input
        .bytes()
        .map(|b| match b {
            b'L' => false,
            b'R' => true,
            _ => panic!("Not a direction: {b}"),
        })
        .collect()
}

#[test]
fn check_parse_instructions() {
    assert_eq!(
        parse_instructions("LRLLR"),
        vec![false, true, false, false, true]
    );
}

fn parse_nodes(input: &str) -> HashMap<&str, Crossroads<&str>> {
    input
        .split("\n")
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .map(|l| {
            let (start, to_str) = l.split_once(" = (").expect(&format!("no equals in '{l}'"));
            let (left, right) = to_str.split_once(", ").expect(&format!("comma in '{l}'?"));
            (start, Crossroads(left, right.strip_suffix(")").unwrap()))
        })
        .collect()
}

#[test]
fn check_parse_nodes() {
    assert_eq!(
        parse_nodes("AAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\n"),
        HashMap::from([
            ("AAA", Crossroads("BBB", "BBB")),
            ("BBB", Crossroads("AAA", "ZZZ"))
        ])
    );
}

fn is_start_node(node: &str) -> bool {
    node.bytes().last().unwrap() == b'A'
}

fn is_end_node(node: &str) -> bool {
    node.bytes().last().unwrap() == b'Z'
}

fn least_common_multiple(numbers: Vec<usize>) -> usize {
    numbers
        .into_iter()
        .reduce(|a, b| a * b / greatest_common_divisor(a, b))
        .unwrap()
}

fn greatest_common_divisor(a: usize, b: usize) -> usize {
    if b == 0 {
        a
    } else {
        greatest_common_divisor(b, a % b)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day08/input.txt")?;
    let (instructions_str, nodes_str) = file.split_once("\n\n").unwrap();
    let instructions = parse_instructions(instructions_str);
    let nodes = parse_nodes(nodes_str);

    let mut current_node = "AAA";
    let mut step_count = 0;

    while current_node != "ZZZ" {
        let go_right = instructions[step_count % instructions.len()];
        current_node = nodes[current_node][go_right];
        step_count += 1;
    }

    println!("Part 1: {:?}", step_count);

    let start_nodes = nodes
        .keys()
        .map(|&s| s)
        .filter(|node| is_start_node(node))
        .collect::<Vec<_>>();

    let mut cycle_times = vec![];
    let mut end_offset_lists = vec![];

    for start_node in start_nodes {
        let mut current_node = start_node;
        let mut end_offsets = vec![];
        let mut seen = HashMap::new();

        let mut step_count = 0;

        loop {
            let instruction_index = step_count % instructions.len();

            if let Some(&cycle_offset) = seen.get(&(instruction_index, current_node)) {
                cycle_times.push(step_count - cycle_offset);
                end_offset_lists.push(end_offsets);
                break;
            }
            seen.insert((instruction_index, current_node), step_count);

            let go_right = instructions[instruction_index];
            current_node = nodes[current_node][go_right];
            step_count += 1;

            if is_end_node(current_node) {
                end_offsets.push(step_count)
            }
        }
    }

    for i in 0..cycle_times.len() {
        assert_eq!(vec![cycle_times[i]], end_offset_lists[i]);
    }

    println!("Part 2: {:?}", least_common_multiple(cycle_times));

    Ok(())
}
