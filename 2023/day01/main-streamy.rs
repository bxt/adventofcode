use std::{
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug)]
enum Token {
    Number { value: u8, was_word: bool },
    Separator,
}

#[derive(Clone, Copy, Debug)]
enum State {
    START,
    E,
    EI,
    EIG,
    EIGH,
    F,
    FI,
    FIV,
    FO,
    FOU,
    N,
    NI,
    NIN,
    O,
    ON,
    S,
    SI,
    SE,
    SEV,
    SEVE,
    T,
    TH,
    THR,
    THRE,
    TW,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = File::open("day01/input.txt")?;
    let bytes = BufReader::new(file).bytes();
    let mut state = State::START;
    let mut results = vec![];

    for byte in bytes {
        match (state, byte) {
            (_, Ok(letter @ b'1'..=b'9')) => {
                state = State::START;
                results.push(Token::Number {
                    value: letter - b'0',
                    was_word: false,
                })
            }
            (_, Ok(b'f')) => {
                state = State::F;
            }
            (_, Ok(b's')) => {
                state = State::S;
            }
            (
                State::START
                | State::E
                | State::EI
                | State::EIG
                | State::EIGH
                | State::F
                | State::FI
                | State::FO
                | State::FOU
                | State::N
                | State::NI
                | State::O
                | State::SI
                | State::SE
                | State::SEVE
                | State::T
                | State::TH
                | State::TW,
                Ok(b'e'),
            ) => {
                state = State::E;
            }
            (
                State::START
                | State::E
                | State::EI
                | State::EIG
                | State::EIGH
                | State::F
                | State::FI
                | State::FIV
                | State::FOU
                | State::N
                | State::NIN
                | State::ON
                | State::S
                | State::SI
                | State::SE
                | State::SEV
                | State::T
                | State::TH
                | State::THR
                | State::THRE
                | State::TW,
                Ok(b'n'),
            ) => {
                state = State::N;
            }
            (
                State::START
                | State::E
                | State::EI
                | State::EIG
                | State::EIGH
                | State::FI
                | State::FIV
                | State::FO
                | State::FOU
                | State::N
                | State::NI
                | State::NIN
                | State::O
                | State::ON
                | State::S
                | State::SI
                | State::SE
                | State::SEV
                | State::SEVE
                | State::T
                | State::TH
                | State::THR
                | State::THRE,
                Ok(b'o'),
            ) => {
                state = State::O;
            }
            (
                State::START
                | State::E
                | State::EI
                | State::EIG
                | State::F
                | State::FI
                | State::FIV
                | State::FO
                | State::FOU
                | State::N
                | State::NI
                | State::NIN
                | State::O
                | State::ON
                | State::S
                | State::SI
                | State::SE
                | State::SEV
                | State::SEVE
                | State::T
                | State::TH
                | State::THR
                | State::THRE
                | State::TW,
                Ok(b't'),
            ) => {
                state = State::T;
            }
            (State::START, Ok(b'g' | b'h' | b'i' | b'r' | b'u' | b'v' | b'w' | b'x')) => {}
            (State::E, Ok(b'i')) => {
                state = State::EI;
            }
            (State::E, Ok(b'g' | b'h' | b'r' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::EI, Ok(b'g')) => {
                state = State::EIG;
            }
            (State::EI, Ok(b'h' | b'i' | b'r' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::EIG, Ok(b'h')) => {
                state = State::EIGH;
            }
            (State::EIG, Ok(b'g' | b'i' | b'r' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::EIGH, Ok(b't')) => {
                state = State::T;
                results.push(Token::Number {
                    value: 8,
                    was_word: true,
                })
            }
            (State::EIGH, Ok(b'g' | b'h' | b'i' | b'r' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::F, Ok(b'i')) => {
                state = State::FI;
            }
            (State::F, Ok(b'o')) => {
                state = State::FO;
            }
            (State::F, Ok(b'g' | b'h' | b'r' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::FI, Ok(b'v')) => {
                state = State::FIV;
            }
            (State::FI, Ok(b'g' | b'h' | b'i' | b'r' | b'u' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::FIV, Ok(b'e')) => {
                state = State::E;
                results.push(Token::Number {
                    value: 5,
                    was_word: true,
                })
            }
            (State::FIV, Ok(b'g' | b'h' | b'i' | b'r' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::FO, Ok(b'n')) => {
                state = State::ON;
            }
            (State::FO, Ok(b'u')) => {
                state = State::FOU;
            }
            (State::FO, Ok(b'g' | b'h' | b'i' | b'r' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::FOU, Ok(b'r')) => {
                state = State::START;
                results.push(Token::Number {
                    value: 4,
                    was_word: true,
                })
            }
            (State::FOU, Ok(b'g' | b'h' | b'i' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::N, Ok(b'i')) => {
                state = State::NI;
            }
            (State::N, Ok(b'g' | b'h' | b'r' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::NI, Ok(b'n')) => {
                state = State::NIN;
            }
            (State::NI, Ok(b'g' | b'h' | b'i' | b'r' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::NIN, Ok(b'e')) => {
                state = State::E;
                results.push(Token::Number {
                    value: 9,
                    was_word: true,
                })
            }
            (State::NIN, Ok(b'i')) => {
                state = State::NI;
            }
            (State::NIN, Ok(b'g' | b'h' | b'r' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::O, Ok(b'n')) => {
                state = State::ON;
            }
            (State::O, Ok(b'g' | b'h' | b'i' | b'r' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::ON, Ok(b'e')) => {
                state = State::E;
                results.push(Token::Number {
                    value: 1,
                    was_word: true,
                })
            }
            (State::ON, Ok(b'i')) => {
                state = State::NI;
            }
            (State::ON, Ok(b'g' | b'h' | b'r' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::S, Ok(b'i')) => {
                state = State::SI;
            }
            (State::S, Ok(b'e')) => {
                state = State::SE;
            }
            (State::S, Ok(b'g' | b'h' | b'r' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::SI, Ok(b'x')) => {
                state = State::START;
                results.push(Token::Number {
                    value: 6,
                    was_word: true,
                })
            }
            (State::SI, Ok(b'g' | b'h' | b'i' | b'r' | b'u' | b'v' | b'w')) => {
                state = State::START;
            }
            (State::SE, Ok(b'v')) => {
                state = State::SEV;
            }
            (State::SE, Ok(b'i')) => {
                state = State::EI;
            }
            (State::SE, Ok(b'g' | b'h' | b'r' | b'u' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::SEV, Ok(b'e')) => {
                state = State::SEVE;
            }
            (State::SEV, Ok(b'g' | b'h' | b'i' | b'r' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::SEVE, Ok(b'n')) => {
                state = State::N;
                results.push(Token::Number {
                    value: 7,
                    was_word: true,
                })
            }
            (State::SEVE, Ok(b'i')) => {
                state = State::EI;
            }
            (State::SEVE, Ok(b'g' | b'h' | b'r' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }

            (State::T, Ok(b'h')) => {
                state = State::TH;
            }
            (State::T, Ok(b'w')) => {
                state = State::TW;
            }
            (State::T, Ok(b'g' | b'i' | b'r' | b'u' | b'v' | b'x')) => {
                state = State::START;
            }

            (State::TH, Ok(b'r')) => {
                state = State::THR;
            }
            (State::TH, Ok(b'g' | b'h' | b'i' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::THR, Ok(b'e')) => {
                state = State::THRE;
            }
            (State::THR, Ok(b'g' | b'h' | b'i' | b'r' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::THRE, Ok(b'e')) => {
                state = State::E;
                results.push(Token::Number {
                    value: 3,
                    was_word: true,
                })
            }
            (State::THRE, Ok(b'i')) => {
                state = State::EI;
            }
            (State::THRE, Ok(b'g' | b'h' | b'r' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (State::TW, Ok(b'o')) => {
                state = State::O;
                results.push(Token::Number {
                    value: 2,
                    was_word: true,
                })
            }
            (State::TW, Ok(b'g' | b'h' | b'i' | b'r' | b'u' | b'v' | b'w' | b'x')) => {
                state = State::START;
            }
            (_, Ok(b'\n')) => {
                state = State::START;
                results.push(Token::Separator)
            }
            (_, Ok(0..=9 | 11..=b'd' | b'j'..=b'm' | b'p' | b'q' | b'y'..)) => {}
            (_, Err(e)) => panic!("{:?}", e),
        }
    }

    let mut first_number_part1 = 0;
    let mut last_number_part1 = 0;
    let mut first_number_part2 = 0;
    let mut last_number_part2 = 0;
    let mut first_number_sum_part1: u32 = 0;
    let mut last_number_sum_part1: u32 = 0;
    let mut first_number_sum_part2: u32 = 0;
    let mut last_number_sum_part2: u32 = 0;

    for token in results {
        match token {
            Token::Number { value, was_word } => {
                if !was_word && first_number_part1 == 0 {
                    first_number_part1 = value;
                }
                if first_number_part2 == 0 {
                    first_number_part2 = value;
                }
                if !was_word {
                    last_number_part1 = value;
                }
                last_number_part2 = value;
            }
            Token::Separator => {
                first_number_sum_part1 += u32::from(first_number_part1);
                last_number_sum_part1 += u32::from(last_number_part1);
                first_number_sum_part2 += u32::from(first_number_part2);
                last_number_sum_part2 += u32::from(last_number_part2);

                first_number_part1 = 0;
                last_number_part1 = 0;
                first_number_part2 = 0;
                last_number_part2 = 0;
            }
        }
    }

    println!(
        "part 1: {}",
        first_number_sum_part1 * 10 + last_number_sum_part1
    );
    println!(
        "part 2: {}",
        first_number_sum_part2 * 10 + last_number_sum_part2
    );

    Ok(())
}
