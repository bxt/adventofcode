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
            (_, Ok(b'\n')) => {
                state = State::START;
                results.push(Token::Separator)
            }
            (State::E, Ok(b'i')) => {
                state = State::EI;
            }
            (State::EI, Ok(b'g')) => {
                state = State::EIG;
            }
            (State::EIG, Ok(b'h')) => {
                state = State::EIGH;
            }
            (State::EIGH, Ok(b't')) => {
                state = State::T;
                results.push(Token::Number {
                    value: 8,
                    was_word: true,
                })
            }
            (State::F, Ok(b'i')) => {
                state = State::FI;
            }
            (State::F, Ok(b'o')) => {
                state = State::FO;
            }
            (State::FI, Ok(b'v')) => {
                state = State::FIV;
            }
            (State::FIV, Ok(b'e')) => {
                state = State::E;
                results.push(Token::Number {
                    value: 5,
                    was_word: true,
                })
            }
            (State::FO, Ok(b'n')) => {
                state = State::ON;
            }
            (State::FO, Ok(b'u')) => {
                state = State::FOU;
            }
            (State::FOU, Ok(b'r')) => {
                state = State::START;
                results.push(Token::Number {
                    value: 4,
                    was_word: true,
                })
            }
            (State::N, Ok(b'i')) => {
                state = State::NI;
            }
            (State::NI, Ok(b'n')) => {
                state = State::NIN;
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
            (State::O, Ok(b'n')) => {
                state = State::ON;
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
            (State::S, Ok(b'i')) => {
                state = State::SI;
            }
            (State::S, Ok(b'e')) => {
                state = State::SE;
            }
            (State::SI, Ok(b'x')) => {
                state = State::START;
                results.push(Token::Number {
                    value: 6,
                    was_word: true,
                })
            }
            (State::SE, Ok(b'v')) => {
                state = State::SEV;
            }
            (State::SE, Ok(b'i')) => {
                state = State::EI;
            }
            (State::SEV, Ok(b'e')) => {
                state = State::SEVE;
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
            (State::T, Ok(b'h')) => {
                state = State::TH;
            }
            (State::T, Ok(b'w')) => {
                state = State::TW;
            }
            (State::TH, Ok(b'r')) => {
                state = State::THR;
            }
            (State::THR, Ok(b'e')) => {
                state = State::THRE;
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
            (State::TW, Ok(b'o')) => {
                state = State::O;
                results.push(Token::Number {
                    value: 2,
                    was_word: true,
                })
            }
            (_, Ok(b'f')) => {
                state = State::F;
            }
            (_, Ok(b's')) => {
                state = State::S;
            }
            (_, Ok(b'e')) => {
                state = State::E;
            }
            (_, Ok(b'n')) => {
                state = State::N;
            }
            (_, Ok(b'o')) => {
                state = State::O;
            }
            (_, Ok(b't')) => {
                state = State::T;
            }
            (_, Ok(_)) => {
                state = State::START;
            }
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
