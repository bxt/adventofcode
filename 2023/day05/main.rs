#[derive(Debug)]
struct Rerange {
    source: i64,
    destination: i64,
    length: i64,
}

trait OptionalReranger {
    fn translate(&self, number: i64) -> Option<i64>;
}

trait Reranger {
    fn translate(&self, number: i64) -> i64;
}

impl OptionalReranger for Rerange {
    fn translate(&self, number: i64) -> Option<i64> {
        (number >= self.source && number < self.source + self.length)
            .then_some(number - self.source + self.destination)
    }
}

impl Reranger for Vec<Rerange> {
    fn translate(&self, number: i64) -> i64 {
        self.iter()
            .map(|rerange| rerange.translate(number))
            .find(Option::is_some)
            .unwrap_or(Some(number))
            .unwrap()
    }
}

impl Reranger for Vec<Vec<Rerange>> {
    fn translate(&self, number: i64) -> i64 {
        self.iter().fold(number, |acc, n| n.translate(acc))
    }
}

#[test]
fn check_translate() {
    let rerange1 = Rerange {
        source: 98,
        destination: 50,
        length: 2,
    };
    assert_eq!(rerange1.translate(97), None);
    assert_eq!(rerange1.translate(98), Some(50));
    assert_eq!(rerange1.translate(99), Some(51));
    assert_eq!(rerange1.translate(100), None);
    let rerange2 = Rerange {
        source: 50,
        destination: 52,
        length: 48,
    };
    assert_eq!(rerange2.translate(49), None);
    assert_eq!(rerange2.translate(50), Some(52));
    assert_eq!(rerange2.translate(97), Some(99));
    assert_eq!(rerange2.translate(98), None);
    let both = vec![rerange1, rerange2];
    assert_eq!(both.translate(0), 0);
    assert_eq!(both.translate(49), 49);
    assert_eq!(both.translate(50), 52);
    assert_eq!(both.translate(97), 99);
    assert_eq!(both.translate(98), 50);
    assert_eq!(both.translate(99), 51);
    assert_eq!(both.translate(100), 100);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string("day05/input.txt")?;

    let chunks = file
        .split("\n\n")
        .map(|chunk| {
            chunk
                .split("\n")
                .map(|line| line.trim())
                .filter(|line| !line.is_empty())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let seeds = chunks
        .first()
        .unwrap()
        .first()
        .unwrap()
        .strip_prefix("seeds: ")
        .unwrap()
        .split(" ")
        .map(|s| s.parse::<i64>().unwrap())
        .collect::<Vec<_>>();

    let reranges = chunks
        .iter()
        .skip(1)
        .map(|chunk| {
            chunk
                .iter()
                .skip(1)
                .map(|line| {
                    let (one, two_three) = line.split_once(" ").unwrap();
                    let (two, three) = two_three.split_once(" ").unwrap();
                    let destination = one.parse::<i64>().unwrap();
                    let source = two.parse::<i64>().unwrap();
                    let length = three.parse::<i64>().unwrap();
                    Rerange {
                        source,
                        destination,
                        length,
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    println!(
        "Part 1: {:?}",
        seeds
            .iter()
            .map(|&seed| reranges.translate(seed))
            .min()
            .unwrap()
    );

    Ok(())
}
