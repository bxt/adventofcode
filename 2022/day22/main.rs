use std::collections::HashMap;

#[derive(Debug)]
enum Move {
    Left,
    Right,
    Forward(u32),
}

fn main() {
    let input = std::fs::read_to_string("src/input.txt").unwrap();
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

    // dbg!(moves);
    let mut heading = 0;
    let directions = [(1, 0), (0, -1), (-1, 0), (0, -1)]; // Right, down, left, up
    let mut x = 0;
    let mut y = 0;

    for m in moves {
        match m {
            Move::Left => heading = (heading + 3) % 4,
            Move::Right => heading = (heading + 1) % 4,
            Move::Forward(steps) => {
                for _ in 0..steps {
                    let (dx, dy) = directions[heading];
                    x += dx;
                    y += dy;
                }
            }
        }
    }

    dbg!((x, y));
}
