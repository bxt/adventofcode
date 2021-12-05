#!/usr/bin/env deno run --allow-write --allow-read --allow-net
import {
  COLOR_BLUE_1,
  COLOR_BLUE_4,
  COLOR_GREEN_3,
  Frame,
  GIF,
  mixColors,
  writeText,
} from "../visualisation_utils/mod.ts";
import {
  calculateCoverage,
  countSpotsAboveMinLineCoverage,
  LineCoverage,
  MIN_LINE_COVERAGE,
  parseInput,
} from "./main.ts";

console.log("Calculating...");

const gif = new GIF([]);

const COLOR_BACKGROUND = COLOR_BLUE_1;
const COLOR_LINE = COLOR_BLUE_4;
const COLOR_COVERED = COLOR_GREEN_3;

const COVER_CAP = 5; // educated guess :P

const size = 1000;
const width = size;
const height = size;

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

const parts = [1, 2] as const;

const drawCoverage = (
  frame: Frame,
  lineCoverage: LineCoverage,
  textColor: number,
  part: 1 | 2,
  markSufficientlyCoveredPixels: boolean,
) => {
  frame.drawBox(1, 1, width, height, COLOR_BACKGROUND);

  Object.entries(lineCoverage).forEach(([yString, row]) => {
    Object.entries(row).forEach(([xString, coverage]) => {
      const x = parseInt(xString, 10);
      const y = parseInt(yString, 10);
      const color =
        markSufficientlyCoveredPixels && coverage >= MIN_LINE_COVERAGE
          ? COLOR_COVERED
          : mixColors(
            COLOR_BACKGROUND,
            COLOR_LINE,
            coverage / COVER_CAP,
          );
      frame.setPixelAt(x, y, color);
    });
  });

  const currentValue = countSpotsAboveMinLineCoverage(lineCoverage);

  writeText(frame, `P${part} ${currentValue}`, 16, 16, textColor);
};

parts.forEach((part) => {
  const lineCoverage = calculateCoverage(
    input,
    part === 2,
    (lineCoverage) => {
      const frame = new Frame(width, height, 30);
      drawCoverage(frame, lineCoverage, COLOR_LINE, part, false);
      gif.push(frame);
    },
  );

  {
    const frame = new Frame(width, height, 1400);
    drawCoverage(frame, lineCoverage, COLOR_COVERED, part, true);
    gif.push(frame);
  }
});

console.log("Encoding...");

const bytes = await gif.encode();

console.log("Writing...");

Deno.writeFile("output.gif", bytes);

console.log("Done!");
