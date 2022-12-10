use crate::pixel_map::PixelMap;

#[derive(Debug)]
pub struct Font {
    font_width: i32,
    font_height: i32,
    font_data: Vec<Vec<bool>>,
}

fn letter_position(letter: char) -> i32 {
    match letter {
        'p' | 'P' => 0,
        '0'..='9' => {
            let digit = letter.to_digit(10).unwrap();
            i32::try_from(digit).unwrap() + 1
        }
        '*' => 11,
        _ => panic!("Letter '{}' not supported!", letter),
    }
}

impl Font {
    pub fn from_file(
        font_width: i32,
        font_height: i32,
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

    pub fn line_height(&self) -> i32 {
        self.font_height + 2
    }

    pub fn write_letter(
        &self,
        pixel_map: &mut PixelMap,
        letter: char,
        (x_offset, y_offset): (i32, i32),
        color: u8,
    ) {
        let pos = letter_position(letter);
        for x in 0..(self.font_width - 1) {
            for y in 0..self.font_height {
                let x_index: usize = (x + pos * self.font_width).try_into().unwrap();
                let y_index: usize = y.try_into().unwrap();

                if self.font_data[y_index][x_index] {
                    pixel_map.set((x + x_offset, y + y_offset), color);
                }
            }
        }
    }

    pub fn write_text(
        &self,
        pixel_map: &mut PixelMap,
        text: &str,
        (x_offset, y_offset): (i32, i32),
        color: u8,
    ) {
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
                    self.write_letter(
                        pixel_map,
                        letter,
                        (current_x_offset, current_y_offset),
                        color,
                    );
                    current_x_offset += self.font_width;
                }
            }
        }
    }
}
