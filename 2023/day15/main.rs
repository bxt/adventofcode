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
fn main() -> () {
    let file = std::fs::read_to_string("day15/input.txt").unwrap();

    // let data = file.split(",").collect::<Vec<_>>();

    println!("Part 1: {:?}", hash_all(&file));
}
