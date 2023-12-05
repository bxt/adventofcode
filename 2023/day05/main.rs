use std::iter::once;

#[derive(Debug, PartialEq, Clone, Copy)]
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
fn mk_rerange(source: i64, destination: i64, length: i64) -> Rerange {
    Rerange {
        source,
        destination,
        length,
    }
}

fn chain_reranges(input: Rerange, ontop: Rerange) -> (Vec<Rerange>, Vec<Rerange>) {
    if ontop.source <= input.destination {
        if ontop.source + ontop.length < input.destination + 1 {
            (vec![input], vec![])
        } else {
            let past_ontop = (input.destination + input.length) - (ontop.source + ontop.length);
            if past_ontop <= 0 {
                (
                    vec![],
                    vec![Rerange {
                        source: input.source,
                        destination: input.destination - ontop.source + ontop.destination,
                        length: input.length,
                    }],
                )
            } else {
                (
                    vec![Rerange {
                        source: input.source - past_ontop + input.length,
                        destination: input.destination - past_ontop + input.length,
                        length: past_ontop,
                    }],
                    vec![Rerange {
                        source: input.source,
                        destination: input.destination - ontop.source + ontop.destination,
                        length: input.length - past_ontop,
                    }],
                )
            }
        }
    } else {
        if ontop.source >= input.destination + input.length {
            (vec![input], vec![])
        } else {
            let past_ontop = (input.destination + input.length) - (ontop.source + ontop.length);
            if past_ontop <= 0 {
                (
                    vec![Rerange {
                        source: input.source,
                        destination: input.destination,
                        length: input.length - past_ontop - ontop.length,
                    }],
                    vec![Rerange {
                        source: ontop.source - input.destination + input.source,
                        destination: ontop.destination,
                        length: ontop.length + past_ontop, // +1?
                    }],
                )
            } else {
                (
                    vec![
                        Rerange {
                            source: input.source,
                            destination: input.destination,
                            length: input.length - past_ontop - ontop.length,
                        },
                        Rerange {
                            source: input.source + input.length - past_ontop,
                            destination: input.destination + input.length - past_ontop,
                            length: past_ontop,
                        },
                    ],
                    vec![Rerange {
                        source: ontop.source - input.destination + input.source,
                        destination: ontop.destination,
                        length: ontop.length,
                    }],
                )
            }
        }
    }
}

fn chain_all_reranges(input: Vec<Rerange>, ontop: Vec<Rerange>) -> Vec<Rerange> {
    let mut input_mut = input;
    let mut output = vec![];
    for o in ontop.iter() {
        input_mut = input_mut
            .into_iter()
            .flat_map(|i| {
                let (new_inputs, outputs) = chain_reranges(i, *o);
                output.extend(outputs.into_iter());
                new_inputs.into_iter()
            })
            .collect();
    }
    output.extend(input_mut.into_iter());
    output
}

#[test]
fn check_chain_reranges() {
    assert_eq!(
        chain_reranges(mk_rerange(3, 5, 1), mk_rerange(1, 2, 10)),
        (vec![], vec![mk_rerange(3, 6, 1)])
    );
    assert_eq!(
        chain_reranges(mk_rerange(3, 5, 1), mk_rerange(1, 2, 4)),
        (vec![mk_rerange(3, 5, 1)], vec![])
    );
    assert_eq!(
        chain_reranges(mk_rerange(3, 5, 1), mk_rerange(1, 2, 5)),
        (vec![], vec![mk_rerange(3, 6, 1)])
    );
    assert_eq!(
        chain_reranges(mk_rerange(3, 5, 2), mk_rerange(1, 2, 5)),
        (vec![mk_rerange(4, 6, 1)], vec![mk_rerange(3, 6, 1)])
    );
    assert_eq!(
        chain_reranges(mk_rerange(3, 5, 6), mk_rerange(1, 2, 5)),
        (vec![mk_rerange(4, 6, 5)], vec![mk_rerange(3, 6, 1)])
    );
    assert_eq!(
        chain_reranges(mk_rerange(3, 5, 6), mk_rerange(5, 2, 4)),
        (vec![mk_rerange(7, 9, 2)], vec![mk_rerange(3, 2, 4)])
    );
    assert_eq!(
        chain_reranges(mk_rerange(3, 5, 6), mk_rerange(5, 2, 6)),
        (vec![], vec![mk_rerange(3, 2, 6)])
    );
    assert_eq!(
        chain_reranges(mk_rerange(3, 5, 7), mk_rerange(12, 2, 4)),
        (vec![mk_rerange(3, 5, 7)], vec![])
    );

    assert_eq!(
        chain_reranges(mk_rerange(3, 5, 7), mk_rerange(11, 2, 4)),
        (vec![mk_rerange(3, 5, 6)], vec![mk_rerange(9, 2, 1)])
    );
    assert_eq!(
        chain_reranges(mk_rerange(3, 5, 7), mk_rerange(8, 2, 4)),
        (vec![mk_rerange(3, 5, 3)], vec![mk_rerange(6, 2, 4)])
    );

    assert_eq!(
        chain_reranges(mk_rerange(3, 5, 7), mk_rerange(7, 2, 4)),
        (
            vec![mk_rerange(3, 5, 2), mk_rerange(9, 11, 1)],
            vec![mk_rerange(5, 2, 4)]
        )
    );
}

#[test]
fn check_translate() {
    let rerange1 = mk_rerange(98, 50, 2);
    assert_eq!(rerange1.translate(97), None);
    assert_eq!(rerange1.translate(98), Some(50));
    assert_eq!(rerange1.translate(99), Some(51));
    assert_eq!(rerange1.translate(100), None);
    let rerange2 = mk_rerange(50, 52, 48);
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

#[test]
fn check_chain_all_reranges() {
    let rerange11 = mk_rerange(98, 50, 2);
    let rerange12 = mk_rerange(50, 52, 48);
    assert_eq!(
        chain_all_reranges(vec![mk_rerange(1, 1, 150)], vec![rerange11, rerange12]),
        vec![
            mk_rerange(98, 50, 2),
            mk_rerange(50, 52, 48),
            mk_rerange(1, 1, 49),
            mk_rerange(100, 100, 51),
        ]
    );

    let rerange0 = mk_rerange(0, 0, 100);
    let rerange21 = mk_rerange(15, 0, 37);
    let rerange22 = mk_rerange(52, 37, 2);
    let rerange23 = mk_rerange(0, 39, 15);
    let chain2 = vec![
        vec![rerange0],
        vec![rerange11, rerange12],
        vec![rerange21, rerange22, rerange23],
    ]
    .into_iter()
    .reduce(chain_all_reranges)
    .unwrap();

    assert_eq!(
        chain2,
        vec![
            mk_rerange(98, 35, 2),
            mk_rerange(15, 0, 35),
            mk_rerange(50, 37, 2),
            mk_rerange(0, 39, 15),
            mk_rerange(52, 54, 46),
        ]
    );

    assert_eq!(chain2.translate(79), 81);
    assert_eq!(chain2.translate(14), 53);
    assert_eq!(chain2.translate(55), 57);
    assert_eq!(chain2.translate(13), 52);
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

    let chained = once(vec![mk_rerange(0, 0, 999999999999)])
        .chain(reranges.into_iter())
        .reduce(chain_all_reranges)
        .unwrap();

    println!(
        "Part 1: {:?}",
        seeds
            .iter()
            .map(|&seed| chained.translate(seed))
            .min()
            .unwrap()
    );

    Ok(())
}
