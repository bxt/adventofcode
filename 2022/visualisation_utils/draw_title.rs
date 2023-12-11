use visualisation_utils::canvas::PixelMap;
use visualisation_utils::encoder::LoopEncoder;
use visualisation_utils::font::Font;

struct Config {
    year: u64,
    day: u64,
}

impl Config {
    fn build(args: &[String]) -> Result<Config, String> {
        let args_length = args.len() - 1;
        if args_length != 2 {
            return Err(format!(
                "Must have exactly 2 arguments, given {args_length}"
            ));
        }
        let year = args[1]
            .parse::<u64>()
            .map_err(|e| format!("Invalid year, {e}"))?;
        let day = args[2]
            .parse::<u64>()
            .map_err(|e| format!("Invalid day, {e}"))?;
        Ok(Config { year, day })
    }
}

impl Default for Config {
    fn default() -> Self {
        Self { year: 2023, day: 5 }
    }
}

fn main() -> () {
    let start = std::time::Instant::now();

    let args: Vec<String> = std::env::args().collect();
    let Config { year, day } = Config::build(&args)
        .map_err(|error| {
            println!("Error when parsing arguments: {error}. Falling back to defaults.");
            error
        })
        .unwrap_or_default();

    let font_width = 7;
    let font = Font::from_file(font_width, 9, 83, "../2021/visualisation_utils/font.pbm");
    let pad = 8;

    let text = &format!("{year} {day:02}");

    let width = text.len() * font_width + pad * 2;
    let height = pad * 2 + font.line_height();
    println!("text: {text}, width: {width:?}, height: {height:?}");

    let target_file = format!("target/output-{year}-{day:02}.gif");
    let mut encoder = LoopEncoder::new(&target_file, (width, height));
    let mut pixel_map = PixelMap::new((width, height));

    font.write_text(&mut pixel_map, text, (pad, pad), 2);

    encoder.write(pixel_map.to_vec());

    let elapsed = start.elapsed();
    println!("Took: {:.2?}", elapsed);
}
