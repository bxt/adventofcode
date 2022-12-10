#[derive(Debug)]
pub struct PixelMap {
    dimensions: ((i32, i32), (i32, i32)),
    width: i32,
    pixels: Vec<u8>,
}

impl PixelMap {
    pub fn new(dimensions: ((i32, i32), (i32, i32)), (width, height): (u16, u16)) -> PixelMap {
        let pixel_length = usize::from(width) * usize::from(height);
        let pixels: Vec<u8> = vec![0; usize::try_from(pixel_length).unwrap()];

        PixelMap {
            dimensions,
            width: i32::from(width),
            pixels,
        }
    }

    pub fn set(&mut self, point: (i32, i32), value: u8) {
        let (x, y) = (
            point.0 - self.dimensions.0 .0,
            point.1 - self.dimensions.1 .0,
        );
        let index = usize::try_from(x + y * self.width).unwrap();
        self.pixels[index] = value;
    }

    pub fn to_vec(self) -> Vec<u8> {
        self.pixels
    }
}
