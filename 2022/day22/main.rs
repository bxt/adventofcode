#[derive(Debug)]
struct Row {
    start: usize,
    length: usize,
    walls: Vec<usize>,
}

#[derive(Debug)]
enum Move {
    Left,
    Right,
    Forward(u32),
}

fn parse_input(input: &str) -> (Vec<Row>, Vec<Move>) {
    let (rows_str, moves_str) = input.split_once("\n\n").unwrap();
    let rows = parse_rows(rows_str);
    let moves = parse_moves(moves_str);

    (rows, moves)
}

fn parse_rows(input: &str) -> Vec<Row> {
    input
        .split("\n")
        .map(|line| {
            let mut bs = line.bytes().peekable();
            let mut start = 0;
            while bs.next_if_eq(&b' ').is_some() {
                start += 1;
            }
            let mut length = 0;
            let mut walls = vec![];
            while let Some(b) = bs.next() {
                match b {
                    b'.' => {}
                    b'#' => walls.push(length),
                    _ => {
                        panic!("huh? {b}")
                    }
                }
                length += 1;
            }

            Row {
                start,
                length,
                walls,
            }
        })
        .collect()
}

fn parse_moves(input: &str) -> Vec<Move> {
    let mut moves = vec![];
    for byte in input.bytes() {
        match byte {
            b'L' => moves.push(Move::Left),
            b'R' => moves.push(Move::Right),
            b'0'..=b'9' => moves.push(Move::Forward((byte - b'0').into())),
            _ => {}
        }
    }
    let mut index = 1;
    while index < moves.len() {
        if let (&Move::Forward(num1), &Move::Forward(num2)) = (&moves[index - 1], &moves[index]) {
            moves.remove(index);
            moves[index - 1] = Move::Forward(num1 * 10 + num2);
        }
        index += 1;
    }

    moves
}

fn part1(input: &(Vec<Row>, Vec<Move>)) -> usize {
    let (rows, moves) = input;
    let mut heading = 0; // 0= Right, 1=down, 2=left, 3=up
    let mut x: usize = rows[0].start;
    let mut y: usize = 0;

    for m in moves {
        match m {
            Move::Left => heading = (heading + 3) % 4,
            Move::Right => heading = (heading + 1) % 4,
            Move::Forward(steps) => {
                'moving: for _ in 0..*steps {
                    let (new_x, new_y) = match heading {
                        1 => {
                            // down
                            let mut new_y = y + 1;
                            if new_y >= rows.len() {
                                new_y = 0;
                            }
                            while x < rows[new_y].start
                                || x >= rows[new_y].start + rows[new_y].length
                            {
                                new_y += 1;
                                if new_y >= rows.len() {
                                    new_y = 0;
                                }
                            }
                            (x, new_y)
                        }
                        3 => {
                            // up
                            let mut new_y = y;
                            if new_y == 0 {
                                new_y = rows.len();
                            }
                            new_y -= 1;
                            while x < rows[new_y].start
                                || x >= rows[new_y].start + rows[new_y].length
                            {
                                if new_y == 0 {
                                    new_y = rows.len();
                                }
                                new_y -= 1;
                            }
                            (x, new_y)
                        }
                        0 => {
                            // right
                            let row = &rows[y];
                            let mut new_x = x + 1;
                            if new_x >= row.start + row.length {
                                new_x = row.start;
                            }
                            (new_x, y)
                        }
                        2 => {
                            // left
                            let row = &rows[y];
                            let mut new_x = x;
                            if new_x <= row.start {
                                new_x = row.start + row.length;
                            }
                            new_x -= 1;
                            (new_x, y)
                        }
                        _ => {
                            panic!("heading?!?")
                        }
                    };
                    // dbg!((new_x, new_y));
                    if new_x < rows[new_y].start {
                        panic!("low");
                    }
                    if new_x >= rows[new_y].start + rows[new_y].length {
                        panic!("high");
                    }
                    if rows[new_y].walls.contains(&new_x) {
                        break 'moving;
                    } else {
                        (x, y) = (new_x, new_y);
                    }
                }
            }
        }
    }

    dbg!((x, y));

    1000 * (y + 1) + 4 * (x + 1) + heading
}

#[test]
fn check_part1() {
    assert_eq!(
        part1(&parse_input(
            &std::fs::read_to_string("day22/example.txt").unwrap()
        )),
        6032
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day22/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1); // 65206 too low, 103242 & 164074 too high

    // let part2 = part2(&parsed_input);
    // println!("part 2: {}", part2);

    Ok(())
}
