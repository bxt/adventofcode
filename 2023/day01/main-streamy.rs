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

struct Lexer<I> {
    iter: I,
    state: State,
}

impl<I> Iterator for Lexer<I>
where
    I: Iterator<Item = Result<u8, std::io::Error>>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(byte) = self.iter.next() {
            match (self.state, byte) {
                (_, Ok(letter @ b'1'..=b'9')) => {
                    self.state = State::START;
                    return Some(Token::Number {
                        value: letter - b'0',
                        was_word: false,
                    });
                }
                (_, Ok(b'\n')) => {
                    self.state = State::START;
                    return Some(Token::Separator);
                }
                (State::E, Ok(b'i')) => {
                    self.state = State::EI;
                }
                (State::EI, Ok(b'g')) => {
                    self.state = State::EIG;
                }
                (State::EIG, Ok(b'h')) => {
                    self.state = State::EIGH;
                }
                (State::EIGH, Ok(b't')) => {
                    self.state = State::T;
                    return Some(Token::Number {
                        value: 8,
                        was_word: true,
                    });
                }
                (State::F, Ok(b'i')) => {
                    self.state = State::FI;
                }
                (State::F, Ok(b'o')) => {
                    self.state = State::FO;
                }
                (State::FI, Ok(b'v')) => {
                    self.state = State::FIV;
                }
                (State::FIV, Ok(b'e')) => {
                    self.state = State::E;
                    return Some(Token::Number {
                        value: 5,
                        was_word: true,
                    });
                }
                (State::FO, Ok(b'n')) => {
                    self.state = State::ON;
                }
                (State::FO, Ok(b'u')) => {
                    self.state = State::FOU;
                }
                (State::FOU, Ok(b'r')) => {
                    self.state = State::START;
                    return Some(Token::Number {
                        value: 4,
                        was_word: true,
                    });
                }
                (State::N, Ok(b'i')) => {
                    self.state = State::NI;
                }
                (State::NI, Ok(b'n')) => {
                    self.state = State::NIN;
                }
                (State::NIN, Ok(b'e')) => {
                    self.state = State::E;
                    return Some(Token::Number {
                        value: 9,
                        was_word: true,
                    });
                }
                (State::NIN, Ok(b'i')) => {
                    self.state = State::NI;
                }
                (State::O, Ok(b'n')) => {
                    self.state = State::ON;
                }
                (State::ON, Ok(b'e')) => {
                    self.state = State::E;
                    return Some(Token::Number {
                        value: 1,
                        was_word: true,
                    });
                }
                (State::ON, Ok(b'i')) => {
                    self.state = State::NI;
                }
                (State::S, Ok(b'i')) => {
                    self.state = State::SI;
                }
                (State::S, Ok(b'e')) => {
                    self.state = State::SE;
                }
                (State::SI, Ok(b'x')) => {
                    self.state = State::START;
                    return Some(Token::Number {
                        value: 6,
                        was_word: true,
                    });
                }
                (State::SE, Ok(b'v')) => {
                    self.state = State::SEV;
                }
                (State::SE, Ok(b'i')) => {
                    self.state = State::EI;
                }
                (State::SEV, Ok(b'e')) => {
                    self.state = State::SEVE;
                }
                (State::SEVE, Ok(b'n')) => {
                    self.state = State::N;
                    return Some(Token::Number {
                        value: 7,
                        was_word: true,
                    });
                }
                (State::SEVE, Ok(b'i')) => {
                    self.state = State::EI;
                }
                (State::T, Ok(b'h')) => {
                    self.state = State::TH;
                }
                (State::T, Ok(b'w')) => {
                    self.state = State::TW;
                }
                (State::TH, Ok(b'r')) => {
                    self.state = State::THR;
                }
                (State::THR, Ok(b'e')) => {
                    self.state = State::THRE;
                }
                (State::THRE, Ok(b'e')) => {
                    self.state = State::E;
                    return Some(Token::Number {
                        value: 3,
                        was_word: true,
                    });
                }
                (State::THRE, Ok(b'i')) => {
                    self.state = State::EI;
                }
                (State::TW, Ok(b'o')) => {
                    self.state = State::O;
                    return Some(Token::Number {
                        value: 2,
                        was_word: true,
                    });
                }
                (_, Ok(b'f')) => {
                    self.state = State::F;
                }
                (_, Ok(b's')) => {
                    self.state = State::S;
                }
                (_, Ok(b'e')) => {
                    self.state = State::E;
                }
                (_, Ok(b'n')) => {
                    self.state = State::N;
                }
                (_, Ok(b'o')) => {
                    self.state = State::O;
                }
                (_, Ok(b't')) => {
                    self.state = State::T;
                }
                (_, Ok(_)) => {
                    self.state = State::START;
                }
                (_, Err(e)) => panic!("{:?}", e),
            }
        }

        None
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = File::open("day01/input.txt")?;
    let bytes = BufReader::new(file).bytes();
    let lexer = Lexer {
        iter: bytes,
        state: State::START,
    };

    let mut first_number_part1 = 0;
    let mut last_number_part1 = 0;
    let mut first_number_part2 = 0;
    let mut last_number_part2 = 0;
    let mut first_number_sum_part1: u32 = 0;
    let mut last_number_sum_part1: u32 = 0;
    let mut first_number_sum_part2: u32 = 0;
    let mut last_number_sum_part2: u32 = 0;

    for token in lexer {
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
