#[derive(Debug, PartialEq)]
struct Game<'a> {
    id: u32,
    picks: Vec<Vec<(&'a str, u32)>>,
}

fn parse_line(line: &str) -> Option<Game> {
    let mut line_bits = line.split(": ");
    let id = line_bits.next()?.split(" ").nth(1)?.parse::<u32>().ok()?;

    let picks = line_bits
        .next()?
        .split("; ")
        .map(|pick_str| {
            pick_str
                .split(", ")
                .map(|cubes_str| {
                    let mut cubes_bits = cubes_str.split(" ");
                    let count_str = cubes_bits.next().ok_or("no bit")?;
                    let count = count_str.parse::<u32>().map_err(|err| err.to_string())?;
                    let color = cubes_bits.next().ok_or("no color")?;
                    Ok((color, count))
                })
                .map(|cube| {
                    cube.unwrap_or_else(|err: String| panic!("Ouch: {:?} with {:?}", line, err))
                })
                .collect()
        })
        .collect();

    Some(Game { id, picks })
}

#[test]
fn check_parse_line() {
    assert_eq!(
        parse_line("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"),
        Some(Game {
            id: 1,
            picks: vec![
                vec![("blue", 3), ("red", 4)],
                vec![("red", 1), ("green", 2), ("blue", 6)],
                vec![("green", 2)]
            ]
        })
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
        .map(|line| parse_line(&line).unwrap_or_else(|| panic!("Ouch: {:?}", line)))
        .collect::<Vec<Game>>();

    let possible_game_id_sum: u32 = parsed_lines
        .iter()
        .filter(|game| is_possible(game))
        .map(|game| game.id)
        .sum();

    println!("part 1: {:?}", possible_game_id_sum);

    Ok(())
}
