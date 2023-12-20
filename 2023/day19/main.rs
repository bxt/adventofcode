use std::collections::HashMap;

#[derive(Debug)]
enum Condition {
    BiggerOrEqual(usize, u64),
    Smaller(usize, u64),
    True,
}

impl Condition {
    fn matches(&self, part: &Vec<u64>) -> bool {
        match self {
            &Condition::BiggerOrEqual(register, value) => part[register] >= value,
            &Condition::Smaller(register, value) => part[register] < value,
            &Condition::True => true,
        }
    }
}

fn main() -> () {
    let keys = ["x", "m", "a", "s"];

    let file = std::fs::read_to_string("day19/input.txt").unwrap();

    let (workflows_str, parts_str) = file.split_once("\n\n").unwrap();

    let workflows = workflows_str
        .lines()
        .map(|line| {
            let (name, rules_str_with_postfix) = line.split_once("{").unwrap();
            let rules_str = rules_str_with_postfix.strip_suffix("}").unwrap();
            let rules = rules_str
                .split(",")
                .map(|rule_str| {
                    if let Some((condition_str, target)) = rule_str.split_once(":") {
                        let register = keys
                            .iter()
                            .position(|k| k == &&condition_str[0..=0])
                            .unwrap();
                        let value = condition_str[2..].parse().unwrap();
                        let condition = match &condition_str[1..=1] {
                            "<" => Condition::Smaller(register, value),
                            ">" => Condition::BiggerOrEqual(register, value + 1),
                            _ => panic!("welp: {}", condition_str),
                        };
                        (condition, target)
                    } else {
                        (Condition::True, rule_str)
                    }
                })
                .collect::<Vec<_>>();
            (name, rules)
        })
        .collect::<HashMap<_, _>>();

    let parts = parts_str
        .lines()
        .map(|line| {
            line.strip_prefix("{")
                .unwrap()
                .strip_suffix("}")
                .unwrap()
                .split(",")
                .zip(keys)
                .map(|(n, prefix)| {
                    n.strip_prefix(prefix)
                        .unwrap()
                        .strip_prefix("=")
                        .unwrap()
                        .parse()
                        .unwrap()
                })
                .collect::<Vec<u64>>()
        })
        .collect::<Vec<_>>();

    let accepted_parts = parts
        .iter()
        .filter(|part| {
            let mut workflow_name = "in";
            loop {
                let workflow = workflows.get(workflow_name).unwrap();
                for (condition, target) in workflow {
                    if condition.matches(part) {
                        workflow_name = target;
                        break;
                    }
                }
                if workflow_name == "A" {
                    return true;
                }
                if workflow_name == "R" {
                    return false;
                }
            }
        })
        .collect::<Vec<_>>();

    let accepted_parts_sum = accepted_parts
        .iter()
        .map(|part| part.iter())
        .flatten()
        .sum::<u64>();

    println!("Part 1: {:?}", accepted_parts_sum);
}
