use std::vec;

use strum_macros::EnumString;

#[derive(Debug, Clone, Copy, PartialEq, EnumString)]
enum Direction {
    #[strum(serialize = "<")]
    Left,
    #[strum(serialize = ">")]
    Right,
}

fn parse_input(input: &str) -> Vec<Direction> {
    input
        .split("")
        .filter_map(|letter| letter.parse().ok())
        .collect()
}

const WIDTH: usize = 7;

fn can_place_piece_at(
    field: &Vec<[bool; WIDTH]>,
    piece: &Vec<Vec<i32>>,
    at_x: i32,
    at_y: usize,
) -> bool {
    for (shape_y, piece_row) in piece.iter().rev().enumerate() {
        for shape_x in piece_row {
            let x = shape_x + at_x;
            let is_in_field_horizontal = (0..i32::try_from(WIDTH).unwrap()).contains(&x);
            if !is_in_field_horizontal {
                return false; // going into wall
            }
            let x_usize = usize::try_from(x).unwrap();
            let y = at_y + shape_y;
            let is_in_field_vertical = y < field.len();
            if is_in_field_vertical && field[y][x_usize] {
                return false; // spot occupied by rested pice
            }
        }
    }

    true
}

fn place_piece_at(field: &mut Vec<[bool; WIDTH]>, piece: &Vec<Vec<i32>>, at_x: i32, at_y: usize) {
    for (shape_y, piece_row) in piece.iter().rev().enumerate() {
        for shape_x in piece_row {
            let x = shape_x + at_x;
            let x_usize = usize::try_from(x).unwrap();
            let y = at_y + shape_y;
            while y >= field.len() {
                field.push([false; WIDTH]);
            }
            field[y][x_usize] = true;
        }
    }
}

fn part1(input: &Vec<Direction>) -> usize {
    let mut field: Vec<[bool; WIDTH]> = vec![];
    let mut jet_index = 0;

    let pieces: [Vec<Vec<i32>>; 5] = [
        vec![vec![2, 3, 4, 5]],
        vec![vec![3], vec![2, 3, 4], vec![3]],
        vec![vec![4], vec![4], vec![2, 3, 4]],
        vec![vec![2], vec![2], vec![2], vec![2]],
        vec![vec![2, 3], vec![2, 3]],
    ];

    for piece_index in 0..2022 {
        let piece = &pieces[piece_index % pieces.len()];
        let mut piece_y = field.len() + 4;
        let mut piece_x: i32 = 0;

        while piece_y > 0 && can_place_piece_at(&field, &piece, piece_x, piece_y - 1) {
            piece_y -= 1;

            let direction = input[jet_index % input.len()];
            jet_index += 1;

            let move_x = match direction {
                Direction::Left => -1,
                Direction::Right => 1,
            };

            let new_x = piece_x + move_x;

            if can_place_piece_at(&field, &piece, new_x, piece_y) {
                piece_x = new_x;
            }
        }

        place_piece_at(&mut field, &piece, piece_x, piece_y);
    }

    field.len()
}

#[test]
fn check_part1() {
    assert_eq!(
        part1(&parse_input(">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")),
        3068
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day17/input.txt")?;

    let parsed_input = parse_input(&file);

    let part1 = part1(&parsed_input);
    println!("part 1: {}", part1);

    // let part2 = part2(&parsed_input);
    // println!("part 2: {}", part2);

    Ok(())
}
