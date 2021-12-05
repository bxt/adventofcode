import { chunk, Frame } from "./deps.ts";

export const fontHeight = 9;
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

const letterPosition = (letter: string) => {
  if (letter.match(/p/i)) return 0;
  if (letter.match(/[0-9]/)) return parseInt(letter, 10) + 1;
  if (letter === "*") return 11;
  throw new Error(`Letter "${letter}" not supported!`);
};

export const writeLetter = (
  image: Frame,
  letter: string,
  xOffset: number,
  yOffset: number,
  color: number,
) => {
  const pos = letterPosition(letter);
  for (let x = 0; x < (fontWidth - 1); x++) {
    for (let y = 0; y < fontHeight; y++) {
      if (font[y][x + pos * fontWidth]) {
        image.setPixelAt(x + xOffset, y + yOffset, color);
      }
    }
  }
};

export const writeText = (
  image: Frame,
  text: string,
  xOffset: number,
  yOffset: number,
  color = 0xffffffff,
) => {
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
};
