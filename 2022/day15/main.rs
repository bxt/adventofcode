use core::cmp::max;
use regex::Regex;
use std::{collections::HashSet, io::Write, str::FromStr};

#[derive(Debug)]
struct Scan {
    sensor: (i32, i32),
    beacon: (i32, i32),
}

impl Scan {
    fn beacon_distance(&self) -> u32 {
        let (sx, sy) = self.sensor;
        let (bx, by) = self.beacon;
        sx.abs_diff(bx) + sy.abs_diff(by)
    }
    fn excluded_at(&self, target_y: i32) -> Option<(i32, i32)> {
        let target_distance = self.sensor.1.abs_diff(target_y);
        let beacon_distance = self.beacon_distance();
        (target_distance <= beacon_distance).then(|| {
            let x_extend = i32::try_from(beacon_distance - target_distance).unwrap();
            let sensor_x = self.sensor.0;
            (sensor_x - x_extend, sensor_x + x_extend)
        })
    }
}

impl FromStr for Scan {
    type Err = std::fmt::Error;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let re: Regex = Regex::new(
            r"^Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)$",
        )
        .unwrap();
        let caps = re.captures(string).unwrap();
        Ok(Self {
            sensor: (caps[1].parse().unwrap(), caps[2].parse().unwrap()),
            beacon: (caps[3].parse().unwrap(), caps[4].parse().unwrap()),
        })
    }
}

trait Weavable
where
    Self: Sized,
{
    fn weave_into(&self, selfs: Vec<Self>) -> Vec<Self>;
}

impl Weavable for (i32, i32) {
    fn weave_into(&self, input: Vec<Self>) -> Vec<Self> {
        let mut ranges = Vec::with_capacity(input.len() + 1);
        let mut building_range = *self;
        let mut building_range_added = false;

        for range in input {
            if range.0 < building_range.0 {
                if range.1 + 1 < building_range.0 {
                    ranges.push(range);
                } else {
                    building_range.0 = range.0;
                    building_range.1 = max(range.1, building_range.1);
                }
            } else if range.0 <= building_range.1 + 1 {
                building_range.1 = max(range.1, building_range.1);
            } else {
                if !building_range_added {
                    ranges.push(building_range);
                    building_range_added = true;
                }
                ranges.push(range);
            }
        }

        if !building_range_added {
            ranges.push(building_range);
        }

        ranges
    }
}

fn parse_input(input: &str) -> Vec<Scan> {
    input
        .trim()
        .lines()
        .map(|line| line.parse().unwrap())
        .collect()
}

fn build_ranges(input: &Vec<Scan>, target_y: i32) -> Vec<(i32, i32)> {
    let mut ranges = vec![];

    for scan in input {
        if let Some(range) = scan.excluded_at(target_y) {
            ranges = range.weave_into(ranges);
        }
    }

    ranges
}

fn part1<const TARGET_Y: i32>(input: &Vec<Scan>) -> i32 {
    let ranges = build_ranges(&input, TARGET_Y);
    let blocked_positions: i32 = ranges.into_iter().map(|(a, b)| (b - a + 1)).sum();

    let beacons_on_target: i32 = input
        .iter()
        .filter_map(|s| (s.beacon.1 == TARGET_Y).then_some(s.beacon.0))
        .collect::<HashSet<_>>()
        .len()
        .try_into()
        .unwrap();

    blocked_positions - beacons_on_target
}

#[test]
fn check_part1() {
    let example_scan = Scan {
        sensor: (8, 7),
        beacon: (2, 10),
    };
    assert_eq!(example_scan.beacon_distance(), 9);
    let test_cases = [
        (-3, None),
        (-2, Some((8, 8))),
        (0, Some((6, 10))),
        (6, Some((0, 16))),
        (7, Some((-1, 17))),
        (8, Some((0, 16))),
        (10, Some((2, 14))),
        (16, Some((8, 8))),
        (17, None),
    ];
    for (target, range) in test_cases {
        assert_eq!(example_scan.excluded_at(target), range)
    }

    assert_eq!(
        (1, 3).weave_into(vec![(5, 6), (9, 10)]),
        vec![(1, 3), (5, 6), (9, 10)]
    );
    assert_eq!((1, 3).weave_into(vec![(4, 6)]), vec![(1, 6)]);
    assert_eq!((1, 9).weave_into(vec![(2, 2), (5, 6)]), vec![(1, 9)]);
    assert_eq!(
        (4, 7).weave_into(vec![(1, 3), (5, 6), (8, 9)]),
        vec![(1, 9)]
    );
    assert_eq!((4, 7).weave_into(vec![(5, 9)]), vec![(4, 9)]);
    assert_eq!((5, 9).weave_into(vec![(4, 7)]), vec![(4, 9)]);

    assert_eq!(
        part1::<10>(&parse_input(
            &std::fs::read_to_string("day15/example.txt").unwrap()
        )),
        26
    );
}

fn part2<const MAX: i32>(input: &Vec<Scan>) -> i128 {
    let div = 100_000;
    for target_y in 0..=MAX {
        if target_y % div == 0 {
            print!("{}/{}\r", target_y / div, MAX / div);
            std::io::stdout().flush().unwrap();
        }
        let ranges = build_ranges(&input, target_y);
        // dbg!(ranges.len());
        if ranges.len() == 2 {
            let target_x = ranges[0].1 + 1;
            return i128::from(target_x) * 4000000 + i128::from(target_y);
        }
    }

    unreachable!()
}

#[test]
fn check_part2() {
    assert_eq!(
        part2::<20>(&parse_input(
            &std::fs::read_to_string("day15/example.txt").unwrap()
        ),),
        56000011
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day15/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1::<2_000_000>(&parsed_input);
    println!("part 1: {}", part1);

    let part2 = part2::<4_000_000>(&parsed_input);
    println!("part 2: {}", part2);

    Ok(())
}
