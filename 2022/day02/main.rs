use strum::IntoEnumIterator;
use strum_macros::{EnumString, EnumIter};
use std::str::FromStr;

#[derive(Debug, EnumIter)]
enum RPS {
    Rock,
    Paper,
    Scissors,
}

impl RPS {
    fn intrinsic_score(&self) -> u32 {
        match self {
            RPS::Rock => 1,
            RPS::Paper => 2,
            RPS::Scissors => 3,
        }
    }

    fn against(&self, opponent: &RPS) -> Outcome {
        match (opponent, self) {
            (RPS::Rock,RPS::Rock) => Outcome::Draw,
            (RPS::Rock,RPS::Paper) => Outcome::Win,
            (RPS::Rock,RPS::Scissors) => Outcome::Lose,
            (RPS::Paper,RPS::Rock) => Outcome::Lose,
            (RPS::Paper,RPS::Paper) => Outcome::Draw,
            (RPS::Paper,RPS::Scissors) => Outcome::Win,
            (RPS::Scissors,RPS::Rock) => Outcome::Win,
            (RPS::Scissors,RPS::Paper) => Outcome::Lose,
            (RPS::Scissors,RPS::Scissors) => Outcome::Draw,
        }
    }
}

#[derive(Debug, PartialEq)]
enum Outcome {
    Win,
    Draw,
    Lose,
}

impl Outcome {
    fn intrinsic_score(&self) -> u32 {
        match self {
            Outcome::Win => 6,
            Outcome::Draw => 3,
            Outcome::Lose => 0,
        }
    }
}

#[derive(Debug, EnumString)]
enum OpponentMove {
    A,
    B,
    C,
}


impl OpponentMove {
    fn to_rps(&self) -> RPS {
        match self {
            OpponentMove::A => RPS::Rock,
            OpponentMove::B => RPS::Paper,
            OpponentMove::C => RPS::Scissors,
        }
    }
}

#[derive(Debug, EnumString)]
enum SelfMove {
    X,
    Y,
    Z,
}

impl SelfMove {
    fn to_rps(&self) -> RPS {
        match self {
            SelfMove::X => RPS::Rock,
            SelfMove::Y => RPS::Paper,
            SelfMove::Z => RPS::Scissors,
        }
    }

    fn to_outcome(&self) -> Outcome {
        match self {
            SelfMove::X => Outcome::Lose,
            SelfMove::Y => Outcome::Draw,
            SelfMove::Z => Outcome::Win,
        }
    }
}

type Round = (OpponentMove, SelfMove);

fn parse_input(input: &str) -> Vec<Round> {
    input
    .split("\n")
    .flat_map(|line| parse_line(line)).collect::<Vec<Round>>()
}

fn parse_line(line: &str) -> Result<Round, Box<dyn std::error::Error>> {
    let mut pieces = line.trim().split(" ");
    let opponent_move  = OpponentMove::from_str(pieces.next().ok_or("no opponent move")?)?;
    let self_move  = SelfMove::from_str(pieces.next().ok_or("no self move")?)?;
    Ok((opponent_move, self_move))
}

fn part1(rounds: &Vec<Round>) -> u32 {
    rounds.iter().map(|(opponent_move, self_move)| {
        let opponent_rps = opponent_move.to_rps();
        let self_rps = self_move.to_rps();
        let outcome = self_rps.against(&opponent_rps);
        return outcome.intrinsic_score() + self_rps.intrinsic_score()
    }).sum()
}

#[test]
fn check_part1() {
    let example = "A Y\nB X\nC Z";
    assert_eq!(part1(&parse_input(example)), 15);
}

fn part2(rounds: &Vec<Round>) -> u32 {
    rounds.iter().map(|(opponent_move, self_move)|{
        let opponent_rps = opponent_move.to_rps();
        let outcome = self_move.to_outcome();
        let self_rps = RPS::iter().find(|rps| rps.against(&opponent_rps) == outcome).unwrap();
        return outcome.intrinsic_score() + self_rps.intrinsic_score()
    }).sum()
}

#[test]
fn check_part2() {
    let example = "A Y\nB X\nC Z";
    assert_eq!(part2(&parse_input(example)), 12);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day02/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    let part2 = part2(&parsed_input);
    println!("part 2: {}", part2);

    Ok(())
}
