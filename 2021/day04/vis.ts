#!/usr/bin/env deno run --allow-write --allow-read --allow-net
import { chunk } from "https://deno.land/std@0.116.0/collections/mod.ts";
import { Frame, GIF } from "https://deno.land/x/imagescript@1.2.9/mod.ts";
import { BoardMarks, parseInput, sumUnmarkedFields } from "./main.ts";

const mixColors = (
  color1: number,
  color2: number,
  color2Percentage: number,
): number => {
  const [r1, g1, b1] = Frame.colorToRGB(color1);
  const [r2, g2, b2] = Frame.colorToRGB(color2);
  const color1Percentage = 1 - color2Percentage;
  const r = r1 * color1Percentage + r2 * color2Percentage;
  const g = g1 * color1Percentage + g2 * color2Percentage;
  const b = b1 * color1Percentage + b2 * color2Percentage;
  return Frame.rgbToColor(r, g, b);
};

const fontHeight = 9;
const fontWidth = 7;
const fontTotalWidth = 83;
const fontString = await Deno.readTextFile("font.pbm");
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

const writeLetter = (
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

const writeText = (
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

console.log("Start...");

const gif = new GIF([]);

const { boards, draws } = parseInput(await Deno.readTextFile("input.txt"));

const OVERDRAW_FRAMES = 40;

const COLOR_BACKGROUND = 0x001122ff;
const COLOR_BOARD = 0x003366ff;
const COLOR_CELL = 0x0055AAff;
const COLOR_CELL_DRAWN = 0x0077EEff;
const COLOR_CELL_JUST_FOUND = 0x00BB00ff;
const COLOR_WIN_FOUND = 0x009900ff;
const DECAY_PER_FRAME = 0.075;
const DECAY_PER_FRAME_OVERDRAW = 0.075;

const BOARDS_PER_ROW = 10;

const cellPad = 2;
const cellSize = 14;

const boardPad = 20;
const boardSize = (cellSize + cellPad) * boards[0].length + cellPad;
const boardOuterSize = boardSize + boardPad;
const width = BOARDS_PER_ROW * boardOuterSize + boardPad;
const height = Math.ceil(boards.length / BOARDS_PER_ROW) * boardOuterSize +
  boardPad;

const allBoardMarks: BoardMarks[] = boards.map((board) =>
  board.map((b) => b.map(() => false))
);
const boardWins: (number | undefined)[] = boards.map((_) => undefined);

let firstBoardToWin: number | undefined = undefined;
let lastBoardToWin: number | undefined = undefined;

console.log("Drawing...");

for (
  let drawIndex = 0;
  drawIndex < draws.length + OVERDRAW_FRAMES;
  drawIndex++
) {
  const draw = draws[drawIndex];

  const overdraw = Math.max(drawIndex - draws.length, 0);
  const darkenOverdraw = Math.min(overdraw * DECAY_PER_FRAME_OVERDRAW, 1);
  const applyCellColorOverdraw = (color: number) =>
    mixColors(color, COLOR_CELL_DRAWN, darkenOverdraw);

  const frame = new Frame(width, height);
  frame.drawBox(1, 1, width, height, COLOR_BACKGROUND);

  const drawOnTop: (() => void)[] = [];

  for (let boardIndex = 0; boardIndex < boards.length; boardIndex++) {
    const boardWin = boardWins[boardIndex];

    const boardWonBefore = boardWin === undefined ? 0 : (drawIndex - boardWin);
    const darken =
      (boardIndex === lastBoardToWin || boardIndex === firstBoardToWin)
        ? 0
        : Math.min(boardWonBefore * DECAY_PER_FRAME, 1);
    const applyDarken = (color: number) =>
      mixColors(color, COLOR_BACKGROUND, darken);

    const board = boards[boardIndex];
    const boardMarks = allBoardMarks[boardIndex];

    const boardX = boardIndex % BOARDS_PER_ROW;
    const boardY = Math.floor(boardIndex / BOARDS_PER_ROW);
    const boardXOffset = boardX * boardOuterSize + boardPad;
    const boardYOffset = boardY * boardOuterSize + boardPad;

    frame.drawBox(
      boardXOffset,
      boardYOffset,
      boardSize,
      boardSize,
      applyDarken(COLOR_BOARD),
    );

    for (let row = 0; row < board.length; row++) {
      for (let col = 0; col < board[row].length; col++) {
        const cellXOffset = boardXOffset + col * (cellSize + cellPad) +
          cellPad;
        const cellYOffset = boardYOffset + row * (cellSize + cellPad) +
          cellPad;

        let cellColor = COLOR_CELL;

        if (boardMarks[row][col]) {
          cellColor = COLOR_CELL_DRAWN;
        }

        if (board[row][col] === draw && boardWin === undefined) {
          boardMarks[row][col] = true;
          cellColor = COLOR_CELL_JUST_FOUND;
        }

        frame.drawBox(
          cellXOffset,
          cellYOffset,
          cellSize,
          cellSize,
          applyDarken(cellColor),
        );
      }
    }

    for (let row = 0; row < board.length; row++) {
      const rowIsFilled = boardMarks[row].every((b) => b);

      if (rowIsFilled) {
        if (boardWin === undefined) {
          boardWins[boardIndex] = drawIndex;
          if (firstBoardToWin === undefined) firstBoardToWin = boardIndex;
          lastBoardToWin = boardIndex;
        }

        const cellYOffset = boardYOffset + row * (cellSize + cellPad) +
          cellPad;

        frame.drawBox(
          boardXOffset + cellPad,
          cellYOffset,
          boardSize - 2 * cellPad,
          cellSize,
          applyDarken(applyCellColorOverdraw(COLOR_WIN_FOUND)),
        );
      }
    }

    for (let col = 0; col < board[0].length; col++) {
      const columnIsFilled = boardMarks.every((_, row) => boardMarks[row][col]);
      if (columnIsFilled) {
        if (boardWin === undefined) {
          boardWins[boardIndex] = drawIndex;
          if (firstBoardToWin === undefined) firstBoardToWin = boardIndex;
          lastBoardToWin = boardIndex;
        }

        const cellXOffset = boardXOffset + col * (cellSize + cellPad) +
          cellPad;

        frame.drawBox(
          cellXOffset,
          boardYOffset + cellPad,
          cellSize,
          boardSize - 2 * cellPad,
          applyDarken(applyCellColorOverdraw(COLOR_WIN_FOUND)),
        );
      }
    }

    if (overdraw > 0) {
      if (firstBoardToWin === boardIndex || lastBoardToWin === boardIndex) {
        drawOnTop.push(() => {
          const decayOffsetPerLine = 3;
          const part = firstBoardToWin === boardIndex ? 1 : 2;
          const winDrawIndex = boardWins[boardIndex];
          if (winDrawIndex === undefined) throw new Error("???");
          const winDraw = draws[winDrawIndex];
          const score = sumUnmarkedFields(board, boardMarks);

          const lines = [
            [`P${part}`, COLOR_CELL_JUST_FOUND],
            [`${winDrawIndex}`, COLOR_CELL],
            [`${winDraw} * ${score}`, COLOR_CELL_DRAWN],
            [`${winDraw * score}`, COLOR_CELL_JUST_FOUND],
          ] as const;

          const yOffsetBase = boardYOffset +
            Math.round((2 * cellSize + cellPad - fontHeight) / 2);

          lines.forEach(([text, color], lineNumber) => {
            const darken = (overdraw - lineNumber * decayOffsetPerLine) *
              DECAY_PER_FRAME_OVERDRAW;
            const darkenClamped = 1 - Math.max(Math.min(darken, 1), 0);
            writeText(
              frame,
              text,
              boardXOffset + boardSize + boardPad,
              yOffsetBase + (cellSize + cellPad) * lineNumber,
              mixColors(color, COLOR_BACKGROUND, darkenClamped),
            );
          });
        });
      }
    }
  }

  drawOnTop.forEach((callback) => callback());

  if (drawIndex < draws.length) {
    const text = `${drawIndex < 10 ? "0" : ""}${drawIndex} ${
      draw < 10 ? "0" : ""
    }${draw}`;
    writeText(
      frame,
      text,
      boardPad,
      Math.round((boardPad - fontHeight) / 2),
      COLOR_CELL_JUST_FOUND,
    );
  }

  gif.push(frame);
}

console.log("Encoding...");

const bytes = await gif.encode();

console.log("Writing...");

Deno.writeFile("output.gif", bytes);

console.log("Done!");
