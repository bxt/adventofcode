use std::iter::once;

#[derive(Debug, PartialEq, Clone, Copy)]
struct Rerange {
    source: i64,
    destination: i64,
    length: i64,
}

#[inline(always)]
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
                        length: ontop.length + past_ontop,
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

fn chain_all_reranges(inputs: Vec<Rerange>, ontops: Vec<Rerange>) -> Vec<Rerange> {
    let mut inputs_mut = inputs;
    let mut output = vec![];
    for ontop in ontops.iter() {
        inputs_mut = inputs_mut
            .into_iter()
            .flat_map(|input| {
                let (new_inputs, outputs) = chain_reranges(input, *ontop);
                output.extend(outputs.into_iter());
                new_inputs.into_iter()
            })
            .collect();
    }
    output.extend(inputs_mut.into_iter());
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
    let reranges = vec![
        vec![rerange0],
        vec![rerange11, rerange12],
        vec![rerange21, rerange22, rerange23],
    ]
    .into_iter()
    .reduce(chain_all_reranges)
    .unwrap();

    assert_eq!(
        reranges,
        vec![
            mk_rerange(98, 35, 2),
            mk_rerange(15, 0, 35),
            mk_rerange(50, 37, 2),
            mk_rerange(0, 39, 15),
            mk_rerange(52, 54, 46),
        ]
    );

    let seeds = vec![79, 14, 55, 13];
    let bootstrap_reranges = seeds.into_iter().map(|s| mk_rerange(s, s, 1)).collect();
    let rs = chain_all_reranges(bootstrap_reranges, reranges);
    let mut mapped = rs.into_iter().map(|r| r.destination).collect::<Vec<_>>();
    mapped.sort();
    assert_eq!(mapped, vec![52, 53, 57, 81]);
}

fn find_min_destination(bootstrap_reranges: Vec<Rerange>, reranges: Vec<Vec<Rerange>>) -> i64 {
    let all_reranges = once(bootstrap_reranges).chain(reranges.into_iter());
    let reduced = all_reranges.reduce(chain_all_reranges).unwrap();
    reduced.into_iter().map(|r| r.destination).min().unwrap()
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
                    mk_rerange(source, destination, length)
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let single_seeds_as_reranges = seeds.iter().map(|&s| mk_rerange(s, s, 1)).collect();
    let part1 = find_min_destination(single_seeds_as_reranges, reranges.to_vec());
    println!("Part 1: {:?}", part1);

    let paired_seeds_as_reranges = seeds
        .chunks(2)
        .map(|chunk| {
            if let &[source, length] = chunk {
                mk_rerange(source, source, length)
            } else {
                panic!("at the Disco")
            }
        })
        .collect::<Vec<_>>();

    let part2 = find_min_destination(paired_seeds_as_reranges, reranges);
    println!("Part 2: {:?}", part2);

    Ok(())
}
