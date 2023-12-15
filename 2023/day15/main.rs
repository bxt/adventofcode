use std::iter::repeat;

fn hash(input: &str) -> u8 {
    let mut value = 0u8;
    for byte in input.bytes() {
        if byte != b'\n' {
            value = value.wrapping_add(byte);
            value = value.wrapping_mul(17);
        }
    }
    value
}

#[test]
fn check_hash() {
    assert_eq!(hash("HASH"), 52);
}

fn hash_all(input: &str) -> u64 {
    input.split(",").map(|s| u64::from(hash(s))).sum::<u64>()
}

#[test]
fn check_hash_all() {
    assert_eq!(
        hash_all("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"),
        1320
    );
}

fn find_lens_box_and_index<'a, 'b, 'c>(
    boxes: &'a mut Vec<Vec<(&'b str, u64)>>,
    label: &'c str,
) -> (&'a mut Vec<(&'b str, u64)>, Option<usize>) {
    let hash: u8 = hash(label);
    let box_index = usize::try_from(hash).unwrap();
    let the_box = &mut boxes[box_index];
    let maybe_index = the_box.iter().position(|(l, _)| *l == label);
    (the_box, maybe_index)
}

enum Instruction<'a> {
    Remove(&'a str),
    Set(&'a str, u64),
}

fn parse_instructions(input: &str) -> impl Iterator<Item = Instruction> {
    input.split(",").map(str::trim).map(|instruction_str| {
        if instruction_str.ends_with("-") {
            let label = &instruction_str[0..instruction_str.len() - 1];
            Instruction::Remove(label)
        } else {
            let (label, value_str) = instruction_str.split_once("=").unwrap();
            let value = value_str.parse::<u64>().unwrap();
            Instruction::Set(label, value)
        }
    })
}

fn run_instructions<'a>(
    instructions: impl Iterator<Item = Instruction<'a>>,
) -> Vec<Vec<(&'a str, u64)>> {
    let mut boxes = repeat(Vec::new()).take(256).collect::<Vec<_>>();

    for instruction in instructions {
        match instruction {
            Instruction::Remove(label) => {
                let (the_box, maybe_index) = find_lens_box_and_index(&mut boxes, label);
                if let Some(index) = maybe_index {
                    the_box.remove(index);
                }
            }
            Instruction::Set(label, value) => {
                let (the_box, maybe_index) = find_lens_box_and_index(&mut boxes, label);
                if let Some(index) = maybe_index {
                    the_box[index].1 = value;
                } else {
                    the_box.push((label, value));
                }
            }
        }
    }

    boxes
}

fn parse_and_run_instructions(input: &str) -> Vec<Vec<(&str, u64)>> {
    run_instructions(parse_instructions(input))
}

fn calculate_focus_power(boxes: &Vec<Vec<(&str, u64)>>) -> usize {
    let mut focus_power = 0;

    for (box_index, the_box) in boxes.iter().enumerate() {
        for (lens_index, lens) in the_box.iter().enumerate() {
            let (_, value) = lens;
            focus_power += (1 + box_index) * (1 + lens_index) * usize::try_from(*value).unwrap();
        }
    }

    focus_power
}

fn find_focus_power(input: &str) -> usize {
    let boxes = parse_and_run_instructions(input);
    calculate_focus_power(&boxes)
}

#[test]
fn check_find_focus_power() {
    assert_eq!(
        find_focus_power("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"),
        145
    );
}

fn main() -> () {
    let file = std::fs::read_to_string("day15/input.txt").unwrap();
    println!("Part 1: {:?}", hash_all(&file));
    println!("Part 2: {:?}", find_focus_power(&file));
}
