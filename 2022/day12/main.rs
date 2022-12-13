use std::{
    collections::{HashMap, VecDeque},
    str::FromStr,
};

type Position = (usize, usize);

#[derive(Debug)]
struct HeightMap {
    items: Vec<Vec<u8>>,
    width: usize,
    height: usize,
    start_position: Position,
    end_position: Position,
}

impl HeightMap {
    fn get(&self, (x, y): Position) -> u8 {
        self.items[y][x]
    }

    fn potential_neighbours(&self, around: Position) -> Vec<Position> {
        let (x, y) = around;
        let mut potential_neighbours = Vec::with_capacity(4);
        if x > 0 {
            potential_neighbours.push((x - 1, y));
        }
        if x < self.width - 1 {
            potential_neighbours.push((x + 1, y));
        }
        if y > 0 {
            potential_neighbours.push((x, y - 1));
        }
        if y < self.height - 1 {
            potential_neighbours.push((x, y + 1));
        }

        potential_neighbours
    }

    fn can_go(&self, from: Position, to: Position) -> bool {
        self.get(to) <= self.get(from) + 1
    }
}

impl FromStr for HeightMap {
    type Err = std::fmt::Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut items = vec![];
        let mut start_position = None;
        let mut end_position = None;

        for (y, line) in input.lines().enumerate() {
            let mut line_items = vec![];
            for (x, ch) in line.bytes().enumerate() {
                match ch {
                    b'S' => {
                        line_items.push(0);
                        start_position = Some((x, y))
                    }
                    b'E' => {
                        line_items.push(b'z' - b'a');
                        end_position = Some((x, y))
                    }
                    b'a'..=b'z' => line_items.push(ch - b'a'),
                    _ => panic!("Oy? {ch}"),
                }
            }
            items.push(line_items)
        }

        Ok(Self {
            width: items[0].len(),
            height: items.len(),
            items,
            start_position: start_position.unwrap(),
            end_position: end_position.unwrap(),
        })
    }
}

fn parse_input(input: &str) -> HeightMap {
    input.parse().unwrap()
}

trait DijkstraInput {
    fn neighbours(&self, around: Position) -> Vec<Position>;
    fn get_start(&self) -> Position;
    fn is_target(&self, potential_target: Position) -> bool;
}

impl DijkstraInput for HeightMap {
    fn neighbours(&self, around: Position) -> Vec<Position> {
        self.potential_neighbours(around)
            .into_iter()
            .filter(|&to| self.can_go(around, to))
            .collect()
    }

    fn get_start(&self) -> Position {
        self.start_position
    }

    fn is_target(&self, potential_target: Position) -> bool {
        potential_target == self.end_position
    }
}

struct ReversedHeightMap<'a>(&'a HeightMap);

impl DijkstraInput for ReversedHeightMap<'_> {
    fn neighbours(&self, around: Position) -> Vec<Position> {
        self.0
            .potential_neighbours(around)
            .into_iter()
            .filter(|&to| self.0.can_go(to, around))
            .collect()
    }

    fn get_start(&self) -> Position {
        self.0.end_position
    }

    fn is_target(&self, potential_target: Position) -> bool {
        self.0.get(potential_target) == 0
    }
}

fn dijkstra(input: &impl DijkstraInput) -> u64 {
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

        if input.is_target(current) {
            return current_distance;
        }

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

    unreachable!()
}

fn part1(input: &HeightMap) -> u64 {
    dijkstra(input)
}

#[test]
fn check_part1() {
    assert_eq!(
        part1(&parse_input(
            &std::fs::read_to_string("day12/example.txt").unwrap()
        )),
        31
    );
}

fn part2(input: &HeightMap) -> u64 {
    dijkstra(&ReversedHeightMap(input))
}

#[test]
fn check_part2() {
    assert_eq!(
        part2(&parse_input(
            &std::fs::read_to_string("day12/example.txt").unwrap()
        )),
        29
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day12/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    let part2 = part2(&parsed_input);
    println!("part 2: {}", part2);

    Ok(())
}
