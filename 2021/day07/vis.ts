#!/usr/bin/env deno run --allow-write --allow-read --allow-net
import { sum } from "../../2020/utils.ts";
import {
  COLOR_BLUE_1,
  COLOR_BLUE_3,
  COLOR_BLUE_4,
  COLOR_GREEN_3,
  COLOR_GREEN_4,
  drawNeatCircle,
  fontHeight,
  fontWidth,
  Frame,
  GIF,
  writeText,
} from "../visualisation_utils/mod.ts";
import { actualFuelConsumption, input } from "./main.ts";

console.log("Calculating...");
console.time("calculation");

const to = Math.max(...input);

const gif = new GIF([]);

const COLOR_BACKGROUND = COLOR_BLUE_1;
const COLOR_CRAB = COLOR_GREEN_3;
const COLOR_CRAB_LINE = COLOR_BLUE_3;
const COLOR_SCANLINE = COLOR_BLUE_4;
const COLOR_SCORE = COLOR_BLUE_4;
const COLOR_BEST_SCORE = COLOR_GREEN_4;
const COLOR_BAR = COLOR_BLUE_3;
const COLOR_BEST_BAR = COLOR_GREEN_3;

const graphHeight = 100;
const graphMargin = 10;
const margin = 20;
const width = to + 2 * margin;
const height = input.length + 2 * graphHeight + 4 * margin;

const valuesByPart: Record<0 | 1, number[]> = [[], []];

for (let target = 0; target <= to; target++) {
  const isLastTarget = target === to;

  const frameDuration = isLastTarget ? 3000 : 10;
  const frame = new Frame(width, height, frameDuration);
  frame.drawBox(1, 1, width, height, COLOR_BACKGROUND);

  input.forEach((position, index) => {
    const x = Math.min(position, target) + margin;
    const y = index + margin;
    const length = Math.abs(position - target);
    frame.drawBox(x, y, length, 1, COLOR_CRAB_LINE);
  });
  input.forEach((position, index) => {
    const x = position + margin;
    const y = index + margin;
    drawNeatCircle(frame, x, y, 3, COLOR_CRAB);
  });

  frame.drawBox(target + margin, margin, 1, input.length, COLOR_SCANLINE);

  const currentValuePart1 = sum(
    input.map((position) => Math.abs(position - target)),
  );
  const currentValuePart2 = sum(
    input.map((position) => actualFuelConsumption(Math.abs(position - target))),
  );

  valuesByPart[0].push(currentValuePart1);
  valuesByPart[1].push(currentValuePart2);

  for (const part of [0, 1] as const) {
    const score = valuesByPart[part][valuesByPart[part].length - 1];
    const bestScore = Math.min(...valuesByPart[part]);
    const worstScore = Math.max(...valuesByPart[part]);
    const bestScoreTarget = valuesByPart[part].indexOf(bestScore);
    const bestScoreString = `P${part + 1} ${bestScore.toString()}`;
    const scoreString = score.toString();
    const textY = input.length + margin * 2 + (margin + graphHeight) * part;
    const bestTextX = bestScoreTarget + margin;
    const bestScoreWidthWithSpace = (bestScoreString.length + 1) * fontWidth;
    const bestTextXEnd = bestScoreTarget + margin + bestScoreWidthWithSpace;
    const currentTextX = Math.max(target + margin, bestTextXEnd);
    const maxCurrentTextX = width - margin - scoreString.length * fontWidth;
    const maxBestTextX = maxCurrentTextX - bestScoreWidthWithSpace;
    writeText(
      frame,
      bestScoreString,
      Math.min(bestTextX, maxBestTextX),
      textY,
      COLOR_BEST_SCORE,
    );
    if (!isLastTarget) {
      writeText(
        frame,
        scoreString,
        Math.min(currentTextX, maxCurrentTextX),
        textY,
        COLOR_SCORE,
      );
    }

    const graphYFrom = textY + fontHeight + graphMargin;
    const graphYTo = textY + graphHeight;
    for (let graphTarget = 0; graphTarget <= target; graphTarget++) {
      const height = valuesByPart[part][graphTarget] / worstScore *
        (graphYTo - graphYFrom);
      frame.drawBox(
        graphTarget + margin,
        graphYTo - height,
        1,
        Math.ceil(height),
        bestScoreTarget === graphTarget ? COLOR_BEST_BAR : COLOR_BAR,
      );
    }
  }

  gif.push(frame);
}

console.timeEnd("calculation");

console.log("Encoding...");

console.time("encoding");
const bytes = await gif.encode();
console.timeEnd("encoding");

console.log("Writing...");

Deno.writeFile("output.gif", bytes);

console.log("Done!");
