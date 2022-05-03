use std::fs::File;
use std::io;

// A     -------
// D --
// -> do nothing
//
// A     -------
// D    ----------
// -> drop
//
// A     -------
// D ------
// -> move front
//
// A     -------
// D       --
// -> move back, create second part
//
// A     -------
// D         ------
// -> move back
//
// A     -------
// D              --
// -> do nothing
//

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("input.txt")?;

    let mut allowed = vec![(0u32, u32::MAX)];

    for line in file.lines() {
        let mut pieces = line.split("-");
        let from_string = pieces.next().ok_or("no from")?;
        let to_string = pieces.next().ok_or("no to")?;
        let disallow_from: u32 = from_string.parse()?;
        let disallow_to: u32 = to_string.parse()?;
        println!("{} to {}", disallow_from, disallow_to);

        if disallow_from == 0 {
            while allowed[0].1 <= disallow_to {
                allowed.remove(0);
            }
            if allowed[0].0 <= disallow_to {
                allowed[0].0 = disallow_to + 1;
            }
            println!("-----666---- {:?}", allowed[0]);
        } else {
            let mut new_allowed = vec![];

            for pair in allowed.iter_mut() {
                let (allowed_from, allowed_to) = pair;
                println!(
                    "-----1---- {} {} {} {} {} {}",
                    *allowed_from,
                    disallow_from,
                    *allowed_to,
                    disallow_from,
                    allowed_from,
                    allowed_to
                );

                if disallow_from >= *allowed_from && disallow_from <= *allowed_to {
                    if *allowed_to >= disallow_to {
                        println!("-----3----");
                        new_allowed.push((disallow_to + 1, *allowed_to))
                    }

                    println!("-----2----");
                    *allowed_to = disallow_from - 1;
                }
            }

            println!("append {:?}", new_allowed);

            allowed.append(&mut new_allowed);
        }

        println!("{:?}", allowed);
    }

    Ok(())
}
