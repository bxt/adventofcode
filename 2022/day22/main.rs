use strum_macros::FromRepr;

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
#[derive(Debug, FromRepr, Clone, Copy)]
enum Heading {
    Right,
    Down,
    Left,
    Up,
}

impl Heading {
    fn turn_left(self) -> Self {
        Self::from_repr((self as usize + 3) % 4).unwrap()
    }
    fn turn_right(self) -> Self {
        Self::from_repr((self as usize + 1) % 4).unwrap()
    }
}

#[derive(Debug)]
struct Turtle {
    x: usize,
    y: usize,
    heading: Heading,
}

impl Turtle {
    fn turn_left(&mut self) {
        self.heading = self.heading.turn_left();
    }
    fn turn_right(&mut self) {
        self.heading = self.heading.turn_right();
    }
    fn calculate_password(self) -> usize {
        let Self { x, y, heading } = self;
        1000 * (y + 1) + 4 * (x + 1) + heading as usize
    }
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
            let mut line_bytes = line.bytes().peekable();
            let mut start = 0;
            while line_bytes.next_if_eq(&b' ').is_some() {
                start += 1;
            }
            let mut length = 0;
            let mut walls = vec![];
            while let Some(byte) = line_bytes.next() {
                match byte {
                    b'.' => {}
                    b'#' => walls.push(length),
                    _ => panic!("Unknown byte in row: {byte}"),
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

fn step_part1(rows: &Vec<Row>, turtle: &Turtle) -> Turtle {
    let Turtle { x, y, heading } = *turtle;
    match heading {
        Heading::Down => {
            let mut new_y = y + 1;
            if new_y >= rows.len() {
                new_y = 0;
            }
            while x < rows[new_y].start || x >= rows[new_y].start + rows[new_y].length {
                new_y += 1;
                if new_y >= rows.len() {
                    new_y = 0;
                }
            }
            Turtle {
                y: new_y,
                ..*turtle
            }
        }
        Heading::Up => {
            let mut new_y = y;
            if new_y == 0 {
                new_y = rows.len();
            }
            new_y -= 1;
            while x < rows[new_y].start || x >= rows[new_y].start + rows[new_y].length {
                if new_y == 0 {
                    new_y = rows.len();
                }
                new_y -= 1;
            }
            Turtle {
                y: new_y,
                ..*turtle
            }
        }
        Heading::Right => {
            let row = &rows[y];
            let mut new_x = x + 1;
            if new_x >= row.start + row.length {
                new_x = row.start;
            }
            Turtle {
                x: new_x,
                ..*turtle
            }
        }
        Heading::Left => {
            let row = &rows[y];
            let mut new_x = x;
            if new_x <= row.start {
                new_x = row.start + row.length;
            }
            new_x -= 1;
            Turtle {
                x: new_x,
                ..*turtle
            }
        }
    }
}

// Hardcoded for input shape as per fold.txt
fn step_part2(_rows: &Vec<Row>, turtle: &Turtle) -> Turtle {
    let Turtle { x, y, heading } = *turtle;
    match heading {
        Heading::Down => {
            if y == 199 && x < 50 {
                // g -> G
                Turtle {
                    x: x + 100,
                    y: 0,
                    heading: Heading::Down,
                }
            } else if y == 149 && 50 <= x && x < 100 {
                // B -> b
                Turtle {
                    x: 49,
                    y: x + 100,
                    heading: Heading::Left,
                }
            } else if y == 49 && 100 <= x && x < 150 {
                // D -> d
                Turtle {
                    x: 99,
                    y: x - 50,
                    heading: Heading::Left,
                }
            } else {
                Turtle {
                    y: y + 1,
                    ..*turtle
                }
            }
        }
        Heading::Up => {
            if y == 100 && x < 50 {
                // a -> A
                Turtle {
                    x: 50,
                    y: x + 50,
                    heading: Heading::Right,
                }
            } else if y == 0 && 50 <= x && x < 100 {
                // F -> f
                Turtle {
                    x: 0,
                    y: x + 100,
                    heading: Heading::Right,
                }
            } else if y == 0 && 100 <= x && x < 150 {
                // G -> g
                Turtle {
                    x: x - 100,
                    y: 199,
                    heading: Heading::Up,
                }
            } else {
                Turtle {
                    y: y - 1,
                    ..*turtle
                }
            }
        }
        Heading::Right => {
            if x == 149 && y < 50 {
                // E -> e
                Turtle {
                    x: 99,
                    y: 149 - y,
                    heading: Heading::Left,
                }
            } else if x == 99 && 50 <= y && y < 100 {
                // d -> D
                Turtle {
                    x: y + 50,
                    y: 49,
                    heading: Heading::Up,
                }
            } else if x == 99 && 100 <= y && y < 150 {
                // e -> E
                Turtle {
                    x: 149,
                    y: 149 - y,
                    heading: Heading::Left,
                }
            } else if x == 49 && 150 <= y && y < 200 {
                // b -> B
                Turtle {
                    x: y - 100,
                    y: 149,
                    heading: Heading::Up,
                }
            } else {
                Turtle {
                    x: x + 1,
                    ..*turtle
                }
            }
        }
        Heading::Left => {
            if x == 50 && y < 50 {
                // C -> c
                Turtle {
                    x: 0,
                    y: 149 - y,
                    heading: Heading::Right,
                }
            } else if x == 50 && 50 <= y && y < 100 {
                // A -> a
                Turtle {
                    x: y - 50,
                    y: 100,
                    heading: Heading::Up,
                }
            } else if x == 0 && 100 <= y && y < 150 {
                // c -> C
                Turtle {
                    x: 50,
                    y: 149 - y,
                    heading: Heading::Right,
                }
            } else if x == 0 && 150 <= y && y < 200 {
                // f -> F
                Turtle {
                    x: y - 100,
                    y: 0,
                    heading: Heading::Down,
                }
            } else {
                Turtle {
                    x: x - 1,
                    ..*turtle
                }
            }
        }
    }
}

fn do_the_moves(
    rows: &Vec<Row>,
    moves: &Vec<Move>,
    step: impl Fn(&Vec<Row>, &Turtle) -> Turtle,
) -> Turtle {
    let mut turtle = Turtle {
        heading: Heading::Right,
        x: rows[0].start,
        y: 0,
    };

    for m in moves {
        match m {
            Move::Left => turtle.turn_left(),
            Move::Right => turtle.turn_right(),
            Move::Forward(steps) => {
                'moving: for _ in 0..*steps {
                    let new_turtle = step(rows, &turtle);
                    let Turtle { x, y, .. } = new_turtle;
                    if rows[y].walls.contains(&(x - rows[y].start)) {
                        break 'moving;
                    } else {
                        turtle = new_turtle;
                    }
                }
            }
        }
    }

    turtle
}

fn part1(input: &(Vec<Row>, Vec<Move>)) -> usize {
    let (rows, moves) = input;
    do_the_moves(rows, moves, step_part1).calculate_password()
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

fn part2(input: &(Vec<Row>, Vec<Move>)) -> usize {
    let (rows, moves) = input;
    do_the_moves(rows, moves, step_part2).calculate_password()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day22/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    let part2 = part2(&parsed_input);
    println!("part 2: {}", part2); // 92383 too low

    Ok(())
}
