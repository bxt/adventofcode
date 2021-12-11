#!/usr/bin/env deno run --allow-write --allow-read --allow-net
import { Coord, CoordSet } from "../../2020/utils.ts";
import {
  COLOR_BLUE_1,
  COLOR_BLUE_3,
  COLOR_BLUE_4,
  COLOR_GREEN_3,
  COLOR_GREEN_4,
  fontHeight,
  fontWidth,
  Frame,
  GIF,
  mixColors,
  writeText,
} from "../visualisation_utils/mod.ts";
import { input, runFlashesUntil } from "./main.ts";

console.log("Calculating...");
console.time("calculation");

const gif = new GIF([]);

const COLOR_BACKGROUND = COLOR_BLUE_1;
const COLOR_COUNTER = COLOR_BLUE_3;
const COLOR_COUNTER_END = COLOR_GREEN_3;
const COLOR_HEIGHT = COLOR_BLUE_4;
const COLOR_FLASHED = COLOR_GREEN_4;

const boxSize = 15;
const boxPad = 1;
const margin = 20;
const width = input[0].length * (boxPad + boxSize) + 2 * boxPad + 2 * margin;
const height = input.length * (boxPad + boxSize) + 2 * boxPad + 2 * margin;

const inputCount = input[0].length * input.length;

let totalFlashedCount = 0;
let currentStep = 0;
let currentState = input;

const drawFrame = (isPart2Done: boolean, flashed: CoordSet) => {
  const isPart1Done = currentStep >= 100;

  const frameDuration = isPart2Done ? 3000 : 100;
  const frame = new Frame(width, height, frameDuration);
  frame.drawBox(1, 1, width, height, COLOR_BACKGROUND);

  currentState.forEach((line, y) =>
    line.forEach((value, x) => {
      const point: Coord = [x, y];
      const drawX = x * (boxPad + boxSize) + boxPad + margin;
      const drawY = y * (boxPad + boxSize) + boxPad + margin;

      const isFlashed = flashed.has(point);

      const baseColor = mixColors(COLOR_BACKGROUND, COLOR_HEIGHT, value / 9);

      const color = isFlashed ? COLOR_FLASHED : baseColor;
      frame.drawBox(drawX, drawY, boxSize, boxSize, color);
    })
  );

  const textXOffset = margin + boxPad;
  const textYOffset = Math.floor((margin + boxPad - fontHeight) / 2);

  const counterP1 = `P1 ${totalFlashedCount.toString().padStart(4)}`;
  const counterP1Color = isPart1Done ? COLOR_COUNTER_END : COLOR_COUNTER;
  writeText(frame, counterP1, textXOffset, textYOffset, counterP1Color);

  const counterP1Width = fontWidth * 7;
  const counterP2 = `P2 ${currentStep.toString().padStart(3)}`;
  const counterP2Color = isPart2Done ? COLOR_COUNTER_END : COLOR_COUNTER;
  const textP2XOffset = textXOffset + counterP1Width + margin;
  writeText(frame, counterP2, textP2XOffset, textYOffset, counterP2Color);

  gif.push(frame);
};

runFlashesUntil(input, ({ step, flashed, state }) => {
  currentState = state;
  currentStep = step;
  if (step <= 100) {
    totalFlashedCount += flashed.size;
  }
  const isPart2Done = flashed.size === inputCount;
  drawFrame(isPart2Done, flashed);
  if (isPart2Done) return step;
});

console.timeEnd("calculation");

console.log("Encoding...");

console.time("encoding");
const bytes = await gif.encode();
console.timeEnd("encoding");

console.log("Writing...");

Deno.writeFile("output.gif", bytes);

console.log("Done!");
