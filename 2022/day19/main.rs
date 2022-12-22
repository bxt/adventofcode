use regex::Regex;
use std::collections::HashSet;
use strum::EnumCount;
use strum_macros::EnumCount;

#[derive(Copy, Clone, EnumCount)]
enum ResourceType {
    Ore,
    Clay,
    Obsidian,
    Geode,
}

type Resources = [i32; ResourceType::COUNT * 2];

#[derive(Debug)]
struct Blueprint {
    label: u32,
    resource_options: Vec<Resources>,
}

fn parse_input(input: &str) -> Vec<Blueprint> {
    let re = Regex::new(r"Blueprint (\d+):[ \n]+Each ore robot costs (\d+) ore.[ \n]+Each clay robot costs (\d+) ore.[ \n]+Each obsidian robot costs (\d+) ore and (\d+) clay.[ \n]+Each geode robot costs (\d+) ore and (\d+) obsidian.").unwrap();
    re.captures_iter(input)
        .map(|caps| {
            let label = caps[1].parse().unwrap();
            let ore_rbt_ore_cost = caps[2].parse::<i32>().unwrap();
            let clay_rbt_ore_cost = caps[3].parse::<i32>().unwrap();
            let obs_rbt_ore_cost = caps[4].parse::<i32>().unwrap();
            let obs_rbt_clay_cost = caps[5].parse::<i32>().unwrap();
            let geo_rbt_ore_cost = caps[6].parse::<i32>().unwrap();
            let geo_rbt_obs_cost = caps[7].parse::<i32>().unwrap();
            Blueprint {
                label,
                resource_options: vec![
                    [-ore_rbt_ore_cost, 0, 0, 0, 1, 0, 0, 0],
                    [-clay_rbt_ore_cost, 0, 0, 0, 0, 1, 0, 0],
                    [-obs_rbt_ore_cost, -obs_rbt_clay_cost, 0, 0, 0, 0, 1, 0],
                    [-geo_rbt_ore_cost, 0, -geo_rbt_obs_cost, 0, 0, 0, 0, 1],
                ],
            }
        })
        .collect()
}

fn part1(input: &Vec<Blueprint>) -> usize {
    dbg!(input);
    dbg!(input.len());
    64
}

#[test]
fn check_part1() {
    assert_eq!(
        part1(&parse_input(
            &std::fs::read_to_string("day19/example.txt").unwrap()
        )),
        64
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day19/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    // let part2 = part2(&parsed_input);
    // println!("part 2: {}", part2);

    Ok(())
}
