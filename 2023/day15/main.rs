use std::iter::repeat;

fn hash(input: &str) -> u64 {
    let mut value = 0;
    for byte in input.bytes() {
        if byte != b'\n' {
            value += u64::from(byte);
            value *= 17;
            value %= 256
        }
    }
    value
}

#[test]
fn check_hash() {
    assert_eq!(hash("HASH"), 52);
}

fn hash_all(input: &str) -> u64 {
    input.split(",").map(|s| hash(s)).sum::<u64>()
}

#[test]
fn check_hash_all() {
    assert_eq!(
        hash_all("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"),
        1320
    );
}

fn find_focus_power(input: &str) -> usize {
    let mut boxes = repeat(Vec::<(&str, u64)>::new())
        .take(256)
        .collect::<Vec<_>>();

    for instruction_str in input.split(",").map(str::trim) {
        if instruction_str.ends_with("-") {
            let label = &instruction_str[0..instruction_str.len() - 1];
            let hash = hash(label);
            let the_box = &mut boxes[usize::try_from(hash).unwrap()];
            let maybe_index = the_box.iter().position(|(l, _)| *l == label);
            if let Some(index) = maybe_index {
                the_box.remove(index);
            }
        } else {
            let (label, value_str) = instruction_str.split_once("=").unwrap();
            let value = value_str.parse::<u64>().unwrap();
            let hash = hash(label);
            let the_box = &mut boxes[usize::try_from(hash).unwrap()];
            let maybe_index = the_box.iter().position(|(l, _)| *l == label);
            if let Some(index) = maybe_index {
                the_box[index].1 = value;
            } else {
                the_box.push((label, value));
            }
        }
    }

    // dbg!(&boxes);

    let mut focus_power = 0;

    for (box_index, the_box) in boxes.iter().enumerate() {
        for (lens_index, lens) in the_box.iter().enumerate() {
            let (_, value) = lens;
            focus_power += (1 + box_index) * (1 + lens_index) * usize::try_from(*value).unwrap();
        }
    }

    focus_power
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

    // let data = file.split(",").collect::<Vec<_>>();

    println!("Part 1: {:?}", hash_all(&file));

    println!("Part 2: {:?}", find_focus_power(&file));
}
