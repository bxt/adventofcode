use std::collections::HashMap;
use std::hash::Hash;

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

fn parse_nodes(input: &str) -> HashMap<&str, (&str, &str)> {
    input
        .split("\n")
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .map(|l| {
            let (start, to_str) = l.split_once(" = (").expect(&format!("no equals in '{l}'"));
            let (left, right) = to_str.split_once(", ").expect(&format!("comma in '{l}'?"));
            (start, (left, right.strip_suffix(")").unwrap()))
        })
        .collect()
}

#[test]
fn check_parse_nodes() {
    assert_eq!(
        parse_nodes("AAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\n"),
        HashMap::from([("AAA", ("BBB", "BBB")), ("BBB", ("AAA", "ZZZ"))])
    );
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
        current_node = if go_right {
            nodes[current_node].1
        } else {
            nodes[current_node].0
        };
        step_count += 1;
    }

    println!("Part 1: {:?}", step_count);

    let mut current_nodes = nodes
        .keys()
        .map(|&s| s)
        .filter(|node| node.bytes().nth(2).unwrap() == b'A')
        .collect::<Vec<_>>();
    let mut step_count = 0;

    while !current_nodes
        .iter()
        .all(|node| node.bytes().nth(2).unwrap() == b'Z')
    {
        for i in 0..current_nodes.len() {
            let go_right = instructions[step_count % instructions.len()];
            current_nodes[i] = if go_right {
                nodes[current_nodes[i]].1
            } else {
                nodes[current_nodes[i]].0
            };
        }
        if (step_count % 10000000 == 0) {
            dbg!(step_count, current_nodes.len(), &current_nodes);
        }
        step_count += 1;
    }

    println!("Part 2: {:?}", step_count);

    Ok(())
}
