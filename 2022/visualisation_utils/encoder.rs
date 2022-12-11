use gif::{Encoder, Frame, Repeat};
use std::{borrow::Cow, fs::File};

use crate::colors::get_color_map;

pub struct LoopEncoder {
    encoder: Encoder<File>,
    width: u16,
    height: u16,
}

impl LoopEncoder {
    pub fn new(file_path: &str, (input_width, input_height): (usize, usize)) -> LoopEncoder {
        let image = File::create(file_path).unwrap();
        let width = u16::try_from(input_width).unwrap();
        let height = u16::try_from(input_height).unwrap();

        let mut encoder = Encoder::new(image, width, height, get_color_map()).unwrap();
        encoder.set_repeat(Repeat::Infinite).unwrap();
        Self {
            encoder,
            width,
            height,
        }
    }

    pub fn write(&mut self, vec: Vec<u8>) {
        let mut frame = Frame::default();
        frame.width = self.width;
        frame.height = self.height;
        frame.buffer = Cow::Owned(vec);
        self.encoder.write_frame(&frame).unwrap();
    }
}
