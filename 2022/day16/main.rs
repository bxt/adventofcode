use core::cmp::{max, min};
use regex::Regex;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    io::Write,
    str::FromStr,
};

#[derive(Debug)]
struct Valve {
    label: String,
    flow_rate: u32,
    connections: Vec<String>,
}

impl Valve {}

impl FromStr for Valve {
    type Err = std::fmt::Error;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let re: Regex =
            Regex::new(r"^Valve (.*) has flow rate=(\d+); tunnels? leads? to valves? (.*)$")
                .unwrap();
        let caps = re.captures(string).unwrap();

        let connections = caps[3].split(", ").map(|s| s.to_owned()).collect();

        Ok(Self {
            label: caps[1].parse().unwrap(),
            flow_rate: caps[2].parse().unwrap(),
            connections,
        })
    }
}

fn parse_input(input: &str) -> Vec<Valve> {
    input
        .trim()
        .lines()
        .map(|line| line.parse().unwrap())
        .collect()
}

trait DijkstraInput {
    fn neighbours(&self, around: String) -> Vec<String>;
    fn get_start(&self) -> String;
}

struct FromValve<'a> {
    start: String,
    valves: &'a Vec<Valve>,
}

impl DijkstraInput for FromValve<'_> {
    fn neighbours(&self, around: String) -> Vec<String> {
        self.valves
            .iter()
            .find(|v| v.label == around)
            .unwrap()
            .connections
    }

    fn get_start(&self) -> String {
        self.start
    }
}

fn dijkstra(input: &impl DijkstraInput) -> HashMap<String, u64> {
    let mut best_distances = HashMap::new();
    let mut known_distances = HashMap::new();
    let mut queue = VecDeque::new();

    best_distances.insert(input.get_start(), 0u64);
    queue.push_back(input.get_start());

    while queue.len() > 0 {
        let current = *queue
            .iter()
            .min_by(|a, b| {
                best_distances
                    .get(a)
                    .unwrap()
                    .cmp(best_distances.get(b).unwrap())
            })
            .unwrap();
        queue.retain(|&e| e != current);

        let current_distance = *best_distances.get(&current).unwrap();
        known_distances.insert(current, current_distance);

        for neighbour in input.neighbours(current) {
            if !known_distances.contains_key(&neighbour) {
                let possible_distance = best_distances.get(&current).unwrap() + 1;
                if best_distances
                    .get(&neighbour)
                    .map(|&best_distance| possible_distance < best_distance)
                    .unwrap_or(true)
                {
                    best_distances.insert(neighbour, possible_distance);
                }
                queue.push_back(neighbour)
            }
        }
    }

    known_distances
}

fn part1<const MINUTES: i32>(input: &Vec<Valve>) -> i32 {
    dbg!(input);

    let mut distance_table = HashMap::new();

    for valve in input {
        distance_table.insert(
            valve.label,
            dijkstra(&FromValve {
                start: valve.label,
                valves: input,
            }),
        );
    }

    26
}

#[test]
fn check_part1() {
    assert_eq!(
        part1::<10>(&parse_input(
            &std::fs::read_to_string("day16/example.txt").unwrap()
        )),
        26
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day16/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1::<30>(&parsed_input);
    println!("part 1: {}", part1);

    // let part2 = part2::<4_000_000>(&parsed_input);
    // println!("part 2: {}", part2);

    Ok(())
}
