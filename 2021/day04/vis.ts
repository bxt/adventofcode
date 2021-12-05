#!/usr/bin/env deno run --allow-write --allow-read --allow-net

// Inspired by Jari Komppa:
// https://twitter.com/Sol_HSA/status/1467033308177514496

import {
  COLOR_BLUE_1,
  COLOR_BLUE_2,
  COLOR_BLUE_3,
  COLOR_BLUE_4,
  COLOR_GREEN_3,
  COLOR_GREEN_4,
  fontHeight,
  Frame,
  GIF,
  mixColors,
  writeText,
} from "../visualisation_utils/mod.ts";
import { BoardMarks, parseInput, sumUnmarkedFields } from "./main.ts";

console.log("Start...");

const gif = new GIF([]);

const { boards, draws } = parseInput(await Deno.readTextFile("input.txt"));

const OVERDRAW_FRAMES = 40;

const COLOR_BACKGROUND = COLOR_BLUE_1;
const COLOR_BOARD = COLOR_BLUE_2;
const COLOR_CELL = COLOR_BLUE_3;
const COLOR_CELL_DRAWN = COLOR_BLUE_4;
const COLOR_CELL_JUST_FOUND = COLOR_GREEN_4;
const COLOR_WIN_FOUND = COLOR_GREEN_3;
const WIN_SCORE_FRAME_GAP = 20;
const DECAY_PER_FRAME = 0.075;
const DECAY_PER_FRAME_OVERDRAW = 0.075;

const BOARDS_PER_ROW = 10;

const cellPad = 2;
const cellSize = 14;

const boardPad = 20;
const boardSize = (cellSize + cellPad) * boards[0].length + cellPad;
const boardOuterSize = boardSize + boardPad;
const width =
  (BOARDS_PER_ROW < boards.length ? BOARDS_PER_ROW : boards.length) *
    boardOuterSize + boardPad;
const height = Math.ceil(boards.length / BOARDS_PER_ROW) * boardOuterSize +
  boardPad;

const allBoardMarks: BoardMarks[] = boards.map((board) =>
  board.map((b) => b.map(() => false))
);
const boardWins: (number | undefined)[] = boards.map((_) => undefined);

let firstBoardToWin: number | undefined = undefined;
let lastBoardToWin: number | undefined = undefined;

const formatNumber = (number: number): string =>
  number.toString().padStart(2, "0");

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
    const darken = Math.min(boardWonBefore * DECAY_PER_FRAME, 1);
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

    const winDrawIndex = boardWins[boardIndex];
    if (winDrawIndex !== undefined) {
      const afterWin = drawIndex - winDrawIndex;
      if (afterWin > WIN_SCORE_FRAME_GAP) {
        const scoreFrames = afterWin - WIN_SCORE_FRAME_GAP;

        const isTarget = firstBoardToWin === boardIndex ||
          lastBoardToWin === boardIndex;

        drawOnTop.push(() => {
          const decayOffsetPerLine = 3;
          const part = firstBoardToWin === boardIndex
            ? "P1"
            : lastBoardToWin === boardIndex
            ? "P2"
            : "";
          const winDraw = draws[winDrawIndex];
          const score = sumUnmarkedFields(board, boardMarks);

          const lines = [
            [part, COLOR_CELL_JUST_FOUND],
            [`${winDrawIndex}`, COLOR_CELL],
            [`${winDraw} * ${score}`, isTarget ? COLOR_CELL_DRAWN : COLOR_CELL],
            [
              `${winDraw * score}`,
              isTarget ? COLOR_CELL_JUST_FOUND : COLOR_CELL_DRAWN,
            ],
          ] as const;

          const yOffsetBase = boardYOffset +
            Math.round((2 * cellSize + cellPad - fontHeight) / 2);

          lines.forEach(([text, color], lineNumber) => {
            const darken = (scoreFrames - lineNumber * decayOffsetPerLine) *
              DECAY_PER_FRAME_OVERDRAW;
            const darkenClamped = 1 - Math.max(Math.min(darken, 1), 0);
            writeText(
              frame,
              text,
              boardXOffset,
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
    const text = `${formatNumber(drawIndex)} ${formatNumber(draw)}`;
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
