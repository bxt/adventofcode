#!/usr/bin/env deno run --allow-write --allow-read --allow-net
import {
  COLOR_BLUE_1,
  COLOR_BLUE_4,
  COLOR_GREEN_3,
  COLOR_GREEN_4,
  fontHeight,
  Frame,
  GIF,
  mixColors,
  writeText,
} from "../visualisation_utils/mod.ts";
import {
  countSpotsAboveMiniumLineCoverage,
  countTwiceCovered,
  MIN_LINE_COVERAGE,
  parseInput,
} from "./main.ts";
import { assertMaybe, Maybe, SparseCoordArray } from "../../2020/utils.ts";

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

parts.forEach((part) => {
  let finalLineCoverage: Maybe<SparseCoordArray<number>> = null;

  countTwiceCovered(input, part === 2, (lineCoverage) => {
    const frame = new Frame(width, height, 30);
    frame.drawBox(1, 1, width, height, COLOR_BACKGROUND);

    Object.entries(lineCoverage).forEach(([yString, row]) => {
      Object.entries(row).forEach(([xString, coverage]) => {
        const x = parseInt(xString, 10);
        const y = parseInt(yString, 10);
        const color = mixColors(
          COLOR_BACKGROUND,
          COLOR_LINE,
          coverage / COVER_CAP,
        );
        frame.setPixelAt(x, y, color);
      });
    });

    const currentValue = countSpotsAboveMiniumLineCoverage(lineCoverage);

    writeText(frame, `P${part} ${currentValue}`, 10, 10, COLOR_LINE);

    gif.push(frame);

    finalLineCoverage = lineCoverage;
  });

  {
    const lineCoverage = assertMaybe(
      finalLineCoverage,
    ) as unknown as SparseCoordArray<
      number
    >;

    const frame = new Frame(width, height, 1400);
    frame.drawBox(1, 1, width, height, COLOR_BACKGROUND);

    Object.entries(lineCoverage).forEach(([yString, row]) => {
      Object.entries(row).forEach(([xString, coverage]) => {
        const x = parseInt(xString, 10);
        const y = parseInt(yString, 10);
        const color = coverage >= MIN_LINE_COVERAGE ? COLOR_COVERED : mixColors(
          COLOR_BACKGROUND,
          COLOR_LINE,
          coverage / COVER_CAP,
        );
        frame.setPixelAt(x, y, color);
      });
    });

    const currentValue = countSpotsAboveMiniumLineCoverage(lineCoverage);

    writeText(frame, `P${part} ${currentValue}`, 10, 10, COLOR_COVERED);

    gif.push(frame);
  }
});

console.log("Encoding...");

const bytes = await gif.encode();

console.log("Writing...");

Deno.writeFile("output.gif", bytes);

console.log("Done!");
