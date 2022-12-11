use crate::pixel_map::Canvas;

#[derive(Debug)]
pub struct Font {
    font_width: usize,
    font_height: usize,
    font_data: Vec<Vec<bool>>,
}

fn letter_position(letter: char) -> usize {
    match letter {
        'p' | 'P' => 0,
        '0'..='9' => {
            let digit = letter.to_digit(10).unwrap();
            usize::try_from(digit).unwrap() + 1
        }
        '*' => 11,
        _ => panic!("Letter '{}' not supported!", letter),
    }
}

impl Font {
    pub fn from_file(
        font_width: usize,
        font_height: usize,
        font_total_width: usize,
        font_path: &str,
    ) -> Font {
        let font_string = std::fs::read_to_string(font_path).unwrap();
        let font_string_header = format!("P1\n{} {}\n", font_total_width, font_height);

        let font_data = font_string
            .strip_prefix(&font_string_header)
            .unwrap()
            .chars()
            .filter_map(|c| match c {
                '0' => Some(false),
                '1' => Some(true),
                _ => None,
            })
            .collect::<Vec<_>>()
            .chunks(font_total_width)
            .map(|c| c.to_vec())
            .collect();

        Font {
            font_width,
            font_height,
            font_data,
        }
    }

    pub fn line_height(&self) -> usize {
        self.font_height + 2
    }

    pub fn write_letter<C>(
        &self,
        canvas: &mut C,
        letter: char,
        (x_offset, y_offset): (usize, usize),
        color: u8,
    ) where
        C: Canvas<Position = (usize, usize)>,
    {
        let pos = letter_position(letter);
        for x in 0..(self.font_width - 1) {
            for y in 0..self.font_height {
                let x_index = pos * self.font_width + x;
                let y_index = y;

                if self.font_data[y_index][x_index] {
                    canvas.set((x + x_offset, y + y_offset), color);
                }
            }
        }
    }

    pub fn write_text<C>(
        &self,
        canvas: &mut C,
        text: &str,
        (x_offset, y_offset): (usize, usize),
        color: u8,
    ) where
        C: Canvas<Position = (usize, usize)>,
    {
        let mut current_y_offset = y_offset;
        let mut current_x_offset = x_offset;

        for letter in text.chars() {
            match letter {
                ' ' => {
                    current_x_offset += self.font_width;
                }
                '\n' => {
                    current_x_offset = x_offset;
                    current_y_offset += self.line_height();
                    continue;
                }
                _ => {
                    self.write_letter(canvas, letter, (current_x_offset, current_y_offset), color);
                    current_x_offset += self.font_width;
                }
            }
        }
    }
}

pub fn get_font() -> Font {
    Font::from_file(7, 9, 83, "../2021/visualisation_utils/font.pbm")
}
