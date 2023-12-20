use std::collections::HashMap;

const KEYS: [&str; 4] = ["x", "m", "a", "s"];

#[derive(Debug, Clone)]
struct Smaller(usize, u64);

#[derive(Debug, Clone)]
enum Term {
    Or(Vec<Box<Term>>),
    And(Vec<Box<Term>>),
    Not(Box<Term>),
    NonTerminal(String),
    Terminal(Smaller),
    True,
    False,
}

fn parse_terminal(s: &str) -> Term {
    let register = KEYS.iter().position(|k| k == &&s[0..=0]).unwrap();
    let value = s[2..].parse().unwrap();
    match &s[1..=1] {
        "<" => Term::Terminal(Smaller(register, value)),
        ">" => Term::Not(Box::new(Term::Terminal(Smaller(register, value + 1)))),
        _ => panic!("welp: {}", s),
    }
}

fn parse_constant_or_non_terminal(s: &str) -> Term {
    match s {
        "R" => Term::False,
        "A" => Term::True,
        _ => Term::NonTerminal(s.to_string()),
    }
}

fn implies(a: Term, b: Term) -> Term {
    Term::Or(
        [Term::Not(Box::new(a)), b]
            .into_iter()
            .map(Box::new)
            .collect(),
    )
}

fn evaluate(term: &Term, part: &Vec<u64>, non_terminals: &HashMap<String, Term>) -> bool {
    match term {
        Term::Or(terms) => terms
            .into_iter()
            .any(|term| evaluate(term, part, non_terminals)),
        Term::And(terms) => terms
            .into_iter()
            .all(|term| evaluate(term, part, non_terminals)),
        Term::Not(inner_term) => !evaluate(inner_term, part, non_terminals),
        Term::NonTerminal(name) => evaluate(non_terminals.get(name).unwrap(), part, non_terminals),
        Term::Terminal(Smaller(index, value)) => part[*index] < *value,
        Term::True => true,
        Term::False => false,
    }
}

fn main() -> () {
    let file = std::fs::read_to_string("day19/input.txt").unwrap();

    let (workflows_str, parts_str) = file.split_once("\n\n").unwrap();

    let non_terminals = workflows_str
        .lines()
        .map(|line| {
            let (name, rules_str_with_postfix) = line.split_once("{").unwrap();
            let rules_str = rules_str_with_postfix.strip_suffix("}").unwrap();

            let mut rule_strs = rules_str.split(",").collect::<Vec<_>>();
            let last = parse_constant_or_non_terminal(rule_strs.pop().unwrap());

            let rules = rule_strs
                .into_iter()
                .rev()
                .map(|rule_str| {
                    if let Some((condition_str, target)) = rule_str.split_once(":") {
                        let condition = parse_terminal(condition_str);
                        let implication = parse_constant_or_non_terminal(target);
                        (condition, implication)
                    } else {
                        panic!("dkgfh!")
                    }
                })
                .fold(last, |c, (a, b)| {
                    Term::And(vec![
                        Box::new(implies(a.clone(), b)),
                        Box::new(implies(Term::Not(Box::new(a)), c)),
                    ])
                });
            (name.to_string(), rules)
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
                .zip(KEYS)
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
        .filter(|part| evaluate(&Term::NonTerminal("in".to_string()), part, &non_terminals))
        .collect::<Vec<_>>();

    let accepted_parts_sum = accepted_parts
        .iter()
        .map(|part| part.iter())
        .flatten()
        .sum::<u64>();

    println!("Part 1: {:?}", accepted_parts_sum);
}
