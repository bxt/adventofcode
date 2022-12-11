use std::marker::PhantomData;

pub trait Canvas {
    type Position;

    fn set(&mut self, position: Self::Position, value: u8);
}

#[derive(Debug)]
pub struct PixelMap {
    width: usize,
    pixels: Vec<u8>,
}

impl PixelMap {
    pub fn new((width, height): (usize, usize)) -> PixelMap {
        let pixel_length = width * height;
        let pixels: Vec<u8> = vec![0; pixel_length];

        PixelMap { width, pixels }
    }

    pub fn to_vec(self) -> Vec<u8> {
        self.pixels
    }
}

impl Canvas for PixelMap {
    type Position = (usize, usize);

    fn set(&mut self, (x, y): (usize, usize), value: u8) {
        let index = x + y * self.width;
        self.pixels[index] = value;
    }
}

#[derive(Debug)]
pub struct OffsetCanvas<'a, T, C> {
    offset: (T, T),
    canvas: &'a mut C,
}

impl<'a, T, C> OffsetCanvas<'a, T, C> {
    pub fn new(canvas: &'a mut C, offset: (T, T)) -> OffsetCanvas<'a, T, C> {
        OffsetCanvas { offset, canvas }
    }
}

impl<'a, T, C> Canvas for OffsetCanvas<'a, T, C>
where
    C: Canvas<Position = (T, T)>,
    T: std::ops::Add<Output = T> + Copy,
{
    type Position = (T, T);

    fn set(&mut self, point: (T, T), value: u8) {
        let position = (point.0 + self.offset.0, point.1 + self.offset.1);
        self.canvas.set(position, value)
    }
}

#[derive(Debug)]
pub struct MappedCanvas<'a, T, C> {
    canvas: &'a mut C,
    phantom: PhantomData<*const T>,
}

impl<'a, T, C> MappedCanvas<'a, T, C> {
    pub fn new(canvas: &'a mut C) -> MappedCanvas<'a, T, C> {
        MappedCanvas {
            canvas,
            phantom: PhantomData,
        }
    }
}

impl<'a, T, C, F> Canvas for MappedCanvas<'a, T, C>
where
    C: Canvas<Position = (F, F)>,
    F: TryFrom<T> + Copy,
    <F as TryFrom<T>>::Error: std::fmt::Debug,
{
    type Position = (T, T);

    fn set(&mut self, (x, y): (T, T), value: u8) {
        let position = (F::try_from(x).unwrap(), F::try_from(y).unwrap());
        self.canvas.set(position, value)
    }
}

#[cfg(test)]
mod tests {
    use super::{Canvas, MappedCanvas, OffsetCanvas, PixelMap};

    impl Canvas for Vec<u8> {
        type Position = usize;

        fn set(&mut self, index: usize, value: u8) {
            self[index] = value;
        }
    }

    #[test]
    fn check_mapped_canvas() {
        let mut pixel_map = PixelMap::new((2, 1));
        pixel_map.set((0, 0), 42);
        {
            let mut mapped_canvas: MappedCanvas<i32, _> = MappedCanvas::new(&mut pixel_map);
            mapped_canvas.set((1i32, 0i32), 7);
        }
        assert_eq!(pixel_map.to_vec(), vec![42, 7]);
    }

    #[test]
    fn check_offset_canvas_and_pixel_map() {
        let mut pixel_map = PixelMap::new((2, 1));
        pixel_map.set((0, 0), 42);
        {
            let mut offset_canvas = OffsetCanvas::new(&mut pixel_map, (1, 0));
            offset_canvas.set((0, 0), 7);
        }
        assert_eq!(pixel_map.to_vec(), vec![42, 7]);
    }
}
