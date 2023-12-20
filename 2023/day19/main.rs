use std::collections::{HashMap, HashSet};

const KEYS: [&str; 4] = ["x", "m", "a", "s"];

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Smaller(usize, u64);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

fn simplify(term: Term) -> Term {
    match term {
        Term::Or(terms) => {
            let unboxed_terms = terms.into_iter().map(|t| *t).collect::<Vec<Term>>();
            if unboxed_terms.iter().any(|term| *term == Term::True) {
                return Term::True;
            }
            let mut new_terms = HashSet::new();
            for term in unboxed_terms {
                let new_term = simplify(term);
                match new_term {
                    Term::True => return Term::True,
                    Term::False => {}
                    Term::Or(subterms) => {
                        new_terms.extend(subterms);
                    }
                    _ => {
                        new_terms.insert(Box::new(new_term));
                    }
                }
            }
            if new_terms.len() == 0 {
                return Term::False;
            }
            if new_terms.len() == 1 {
                return *new_terms.drain().next().unwrap();
            }
            Term::Or(new_terms.into_iter().collect())
        }
        Term::And(terms) => {
            let unboxed_terms = terms.into_iter().map(|t| *t).collect::<Vec<Term>>();
            if unboxed_terms.iter().any(|term| *term == Term::False) {
                return Term::False;
            }
            let mut new_terms = HashSet::new();
            for term in unboxed_terms {
                let new_term = simplify(term);
                match new_term {
                    Term::False => return Term::False,
                    Term::True => {}
                    Term::And(subterms) => {
                        new_terms.extend(subterms);
                    }
                    _ => {
                        new_terms.insert(Box::new(new_term));
                    }
                }
            }
            if new_terms.len() == 0 {
                return Term::True;
            }
            if new_terms.len() == 1 {
                return *new_terms.drain().next().unwrap();
            }
            Term::And(new_terms.into_iter().collect())
        }
        Term::Not(inner_term) => match simplify(*inner_term) {
            Term::False => Term::True,
            Term::True => Term::False,
            Term::Not(inner_inner_term) => *inner_inner_term,
            something_else => Term::Not(Box::new(something_else)),
        },
        other => other,
    }
}

fn resolve_non_terminals(term: Term, non_terminals: &HashMap<String, Term>) -> Term {
    match term {
        Term::Or(terms) => Term::Or(
            terms
                .into_iter()
                .map(|term| Box::new(resolve_non_terminals(*term, &non_terminals)))
                .collect(),
        ),
        Term::And(terms) => Term::And(
            terms
                .into_iter()
                .map(|term| Box::new(resolve_non_terminals(*term, &non_terminals)))
                .collect(),
        ),
        Term::Not(inner_term) => {
            Term::Not(Box::new(resolve_non_terminals(*inner_term, &non_terminals)))
        }
        Term::NonTerminal(name) => {
            resolve_non_terminals(non_terminals.get(&name).unwrap().clone(), &non_terminals)
        }
        _ => term,
    }
}

fn count_subterms(term: &Term) -> usize {
    match term {
        Term::Or(terms) | Term::And(terms) => {
            1 + terms.iter().map(|term| count_subterms(term)).sum::<usize>()
        }

        Term::Not(inner_term) => 1 + count_subterms(inner_term),
        _ => 1,
    }
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

    // dbg!(&non_terminals);

    dbg!(non_terminals
        .values()
        .map(|v| count_subterms(v))
        .sum::<usize>());

    let non_terminals = non_terminals
        .into_iter()
        .map(|(k, v)| (k, simplify(v)))
        .collect::<HashMap<_, _>>();

    dbg!(non_terminals
        .values()
        .map(|v| count_subterms(v))
        .sum::<usize>());

    let main_term = simplify(resolve_non_terminals(
        non_terminals.get("in").unwrap().clone(),
        &non_terminals,
    ));

    dbg!(count_subterms(&main_term));

    let main_term = simplify(main_term);

    dbg!(count_subterms(&main_term));

    // dbg!(&main_term);

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
        .filter(|part| evaluate(&main_term, part, &non_terminals))
        .collect::<Vec<_>>();

    let accepted_parts_sum = accepted_parts
        .iter()
        .map(|part| part.iter())
        .flatten()
        .sum::<u64>();

    println!("Part 1: {:?}", accepted_parts_sum);
}
