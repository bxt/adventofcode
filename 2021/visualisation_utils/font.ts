import { chunk, Frame } from "./deps.ts";

/**
 * Height of the letters in the font
 */
export const fontHeight = 9;
/**
 * Width of the letters in the font
 */
export const fontWidth = 7;
const fontTotalWidth = 83;

const fontPath = new URL("font.pbm", import.meta.url);
const fontString = await Deno.readTextFile(fontPath);

const fontStringHeader = `P1\n${fontTotalWidth} ${fontHeight}\n`;
if (!fontString.startsWith(fontStringHeader)) {
  throw new Error("Font size not supported");
}

const font = chunk(
  fontString.substring(fontStringHeader.length).split("").filter(
    (c) => c === "0" || c === "1",
  ).map((c) => parseInt(c, 2)),
  fontTotalWidth,
);

function letterPosition(letter: string) {
  if (letter.match(/p/i)) return 0;
  if (letter.match(/[0-9]/)) return parseInt(letter, 10) + 1;
  if (letter === "*") return 11;
  throw new Error(`Letter "${letter}" not supported!`);
}

/**
 * Write a single letter to a position in the image.
 * Only "P", 0-9 and "*" are supported.
 * Will throw an `Error` for unsupported letter.
 */
export function writeLetter(
  image: Frame,
  letter: string,
  xOffset: number,
  yOffset: number,
  color: number,
) {
  const pos = letterPosition(letter);
  for (let x = 0; x < (fontWidth - 1); x++) {
    for (let y = 0; y < fontHeight; y++) {
      if (font[y][x + pos * fontWidth]) {
        image.setPixelAt(x + xOffset, y + yOffset, color);
      }
    }
  }
}

/**
 * Write a bunch of letters to a position in the image. In additon to the
 * letters supported by `writeLetter`, new lines and spaces are supported.
 */
export function writeText(
  image: Frame,
  text: string,
  xOffset: number,
  yOffset: number,
  color = 0xffffffff,
) {
  let currentYOffset = yOffset;
  let currentXOffset = xOffset;

  for (let i = 0; i < text.length; i++) {
    const letter = text.charAt(i);

    if (letter === " ") {
      currentXOffset += fontWidth;
    } else if (letter === "\n") {
      currentXOffset = xOffset;
      currentYOffset += fontHeight + 2;
      continue;
    } else {
      writeLetter(image, letter, currentXOffset, currentYOffset, color);
      currentXOffset += fontWidth;
    }
  }
}
