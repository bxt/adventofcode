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

    fn neighbours(&self, around: Position) -> Vec<Position> {
        let (x, y) = around;
        let mut potential_neighbours = vec![];
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

        let around_value = self.get(around);

        potential_neighbours
            .into_iter()
            .filter(|&pos| self.get(pos) <= around_value + 1)
            .collect()
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

fn part1(input: &HeightMap) -> u64 {
    println!("{input:?}");

    // let mut best_distances = RefCell::new(HashMap::new());
    let mut best_distances = HashMap::new();
    let mut known_distances = HashMap::new();
    let mut queue = VecDeque::new();

    best_distances.insert(input.start_position, 0u64);
    queue.push_back(input.start_position);

    while queue.len() > 0 {
        // println!("queue: {queue:?}");

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

        // println!("current: {current:?}");
        // println!("best_distances: {best_distances:?}");
        // println!("known_distances: {known_distances:?}");
        // println!("-------");

        let current_distance = *best_distances.get(&current).unwrap();
        known_distances.insert(current, current_distance);

        for neighbour in input.neighbours(current) {
            if !known_distances.contains_key(&neighbour) {
                let possible_distance = best_distances.get(&current).unwrap() + 1;
                if best_distances
                    .get(&neighbour)
                    .map(|&d| possible_distance < d)
                    .unwrap_or(true)
                {
                    best_distances.insert(neighbour, possible_distance);
                }
                queue.push_back(neighbour)
            }
        }
    }

    *known_distances.get(&input.end_position).unwrap()
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day12/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    // let part2 = part2(&parsed_input);
    // println!("part 2: {}", part2);

    Ok(())
}
