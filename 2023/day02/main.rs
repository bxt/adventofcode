#[derive(Debug, PartialEq)]
struct Game<'a> {
    id: u32,
    picks: Vec<Vec<(&'a str, u32)>>,
}

fn parse_line(line: &str) -> Game {
    let mut line_strs = line.split(": ");
    let id_str = line_strs.next().unwrap().split(" ").nth(1).unwrap();
    let id = id_str.parse::<u32>().unwrap();

    let pick_strs = line_strs.next().unwrap().split("; ");
    let picks = pick_strs
        .map(|pick_str| {
            pick_str
                .split(", ")
                .map(|cubes_str| {
                    let mut cubes_strs = cubes_str.split(" ");
                    let count = cubes_strs.next().unwrap().parse::<u32>().unwrap();
                    let color = cubes_strs.next().unwrap();
                    (color, count)
                })
                .collect()
        })
        .collect();

    Game { id, picks }
}

#[test]
fn check_parse_line() {
    assert_eq!(
        parse_line("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"),
        Game {
            id: 1,
            picks: vec![
                vec![("blue", 3), ("red", 4)],
                vec![("red", 1), ("green", 2), ("blue", 6)],
                vec![("green", 2)]
            ]
        }
    );
}

fn is_possible(game: &Game) -> bool {
    game.picks.iter().all(|pick| {
        pick.iter().all(|(color, count)| match *color {
            "red" => *count <= 12,
            "green" => *count <= 13,
            "blue" => *count <= 14,
            _ => true,
        })
    })
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day02/input.txt")?;

    let parsed_lines = file
        .split("\n")
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .map(|line| parse_line(&line))
        .collect::<Vec<Game>>();

    let possible_game_id_sum: u32 = parsed_lines
        .iter()
        .filter(|game| is_possible(game))
        .map(|game| game.id)
        .sum();

    println!("part 1: {:?}", possible_game_id_sum);

    Ok(())
}
