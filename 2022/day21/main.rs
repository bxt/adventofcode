use std::collections::HashMap;

fn parse_input(input: &str) -> (HashMap<&str, i128>, Vec<(&str, &str, &str, &str)>) {
    let mut known = HashMap::new();
    let mut unknown = vec![];
    for line in input.trim().lines() {
        let (var, value) = line.split_once(": ").unwrap();
        if let Ok(number) = value.parse::<i128>() {
            known.insert(var, number);
        } else {
            let (fst, op_and_snd) = value.split_once(" ").unwrap();
            let (op, snd) = op_and_snd.split_once(" ").unwrap();
            unknown.push((var, fst, op, snd));
        }
    }
    (known, unknown)
}

fn part1(input: &str) -> i128 {
    let (mut known, mut unknown) = parse_input(input);

    dbg!(known.len());
    while !known.contains_key("root") {
        unknown = unknown
            .into_iter()
            .filter(|(var, fst, op, snd)| {
                if known.contains_key(fst) && known.contains_key(snd) {
                    let fst_val = known.get(fst).unwrap();
                    let snd_val = known.get(snd).unwrap();
                    known.insert(
                        var,
                        match op {
                            &"+" => fst_val + snd_val,
                            &"-" => fst_val - snd_val,
                            &"*" => fst_val * snd_val,
                            &"/" => fst_val / snd_val,
                            _ => panic!("unknown op: {op}"),
                        },
                    );
                    return false;
                }
                return true;
            })
            .collect();
        dbg!(known.len());
    }

    *known.get("root").unwrap()
}

fn part2(input: &str) -> i128 {
    let (mut known, mut unknown) = parse_input(input);

    for line in input.trim().lines() {
        let (var, value) = line.split_once(": ").unwrap();
        if let Ok(number) = value.parse::<i128>() {
            known.insert(var, number);
        } else {
            let (fst, op_and_snd) = value.split_once(" ").unwrap();
            let (op, snd) = op_and_snd.split_once(" ").unwrap();
            unknown.push((var, fst, op, snd));
        }
    }
    dbg!(known.len());
    known.remove("humn");
    dbg!(known.len());
    while !known.contains_key("humn") {
        unknown = unknown
            .into_iter()
            .filter(|(var, fst, op, snd)| {
                if var == &"root" {
                    if known.contains_key(fst) || known.contains_key(snd) {
                        if known.contains_key(fst) {
                            let fst_val = known.get(fst).unwrap();
                            known.insert(snd, *fst_val);
                        } else {
                            let snd_val = known.get(snd).unwrap();
                            known.insert(fst, *snd_val);
                        }
                        return false;
                    }
                } else if known.contains_key(fst) && known.contains_key(snd) {
                    let fst_val = known.get(fst).unwrap();
                    let snd_val = known.get(snd).unwrap();
                    known.insert(
                        var,
                        match op {
                            &"+" => fst_val + snd_val,
                            &"-" => fst_val - snd_val,
                            &"*" => fst_val * snd_val,
                            &"/" => fst_val / snd_val,
                            _ => panic!("unknown op: {op}"),
                        },
                    );
                    return false;
                } else if known.contains_key(var) && known.contains_key(snd) {
                    let var_val = known.get(var).unwrap();
                    let snd_val = known.get(snd).unwrap();
                    known.insert(
                        fst,
                        match op {
                            &"+" => var_val - snd_val,
                            &"-" => var_val + snd_val,
                            &"*" => var_val / snd_val,
                            &"/" => var_val * snd_val,
                            _ => panic!("unknown op: {op}"),
                        },
                    );
                    return false;
                } else if known.contains_key(var) && known.contains_key(fst) {
                    let var_val = known.get(var).unwrap();
                    let fst_val = known.get(fst).unwrap();
                    known.insert(
                        snd,
                        match op {
                            &"+" => var_val - fst_val,
                            &"-" => fst_val - var_val,
                            &"*" => var_val / fst_val,
                            &"/" => fst_val / var_val,
                            _ => panic!("unknown op: {op}"),
                        },
                    );
                    return false;
                }
                return true;
            })
            .collect();
        dbg!(known.len());
    }

    *known.get("humn").unwrap()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day21/input.txt")?;

    let part1 = part1(&file);
    println!("part 1: {}", part1);

    let part2 = part2(&file);
    println!("part 2: {}", part2);

    Ok(())
}
