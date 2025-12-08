#!/usr/bin/env deno run --allow-write --allow-read --allow-net
import {
  chunk,
  sortBy,
} from "jsr:@std/collections@1.1.3"
import { Coord, CoordSet, indexWithCoord } from "../../2020/utils.ts";
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
import {
  findLowPoints,
  findNeighbors,
  input,
  largestBasinRating,
  lowPointRiskLevel,
} from "./main.ts";

console.log("Calculating...");
console.time("calculation");

const gif = new GIF([]);

const COLOR_BACKGROUND = COLOR_BLUE_1;
const COLOR_COUNTER = COLOR_BLUE_3;
const COLOR_COUNTER_END = COLOR_GREEN_3;
const COLOR_HEIGHT = COLOR_BLUE_4;
const COLOR_BASIN_POINT = COLOR_GREEN_3;
const COLOR_LOW_POINT = COLOR_GREEN_4;
const COLOR_FRONTIER_POINT = COLOR_GREEN_4;

const boxSize = 9;
const boxPad = 1;
const margin = 20;
const width = input[0].length * (boxPad + boxSize) + 2 * boxPad + 2 * margin;
const height = input.length * (boxPad + boxSize) + 2 * boxPad + 2 * margin;

const lowPoints = findLowPoints(input);

const currentFrontiers = lowPoints.map((_) => new CoordSet());
const currentBasins = lowPoints.map((_) => new CoordSet());

const drawFrame = (isPart2Done = false) => {
  const isPart1Done = currentLowPoints.length === lowPoints.length;

  const frameDuration = isPart2Done ? 3000 : isPart1Done ? 200 : 100;
  const frame = new Frame(width, height, frameDuration);
  frame.drawBox(1, 1, width, height, COLOR_BACKGROUND);

  const biggestBasins = sortBy(currentBasins, (b) => -b.size).slice(0, 3);

  input.forEach((line, y) =>
    line.forEach((value, x) => {
      const point: Coord = [x, y];
      const drawX = x * (boxPad + boxSize) + boxPad + margin;
      const drawY = y * (boxPad + boxSize) + boxPad + margin;

      const isLowPoint = currentLowPoints.some(([px, py]) =>
        px === x && py === y
      );
      const isBasinPoint = currentBasins.some((basin) => basin.has(point));
      const isFrontierPoint = currentFrontiers.some((basin) =>
        basin.has(point)
      );
      const isBiggestBasinPont = biggestBasins.some((b) => b.has(point));

      const baseColor = mixColors(COLOR_BACKGROUND, COLOR_HEIGHT, value / 9);

      const color = isLowPoint ? COLOR_LOW_POINT : isBiggestBasinPont
        ? COLOR_BASIN_POINT
        : isFrontierPoint
        ? mixColors(COLOR_FRONTIER_POINT, baseColor, 0.3)
        : isBasinPoint
        ? mixColors(COLOR_BASIN_POINT, baseColor, 0.3)
        : baseColor;
      frame.drawBox(drawX, drawY, boxSize, boxSize, color);
    })
  );

  const textXOffset = margin + boxPad;
  const textYOffset = Math.floor((margin + boxPad - fontHeight) / 2);

  const counterP1 = `P1 ${
    lowPointRiskLevel(input, currentLowPoints).toString().padStart(4)
  }`;
  const counterP1Color = isPart1Done ? COLOR_COUNTER_END : COLOR_COUNTER;
  writeText(frame, counterP1, textXOffset, textYOffset, counterP1Color);

  if (isPart1Done) {
    const counterP1Width = fontWidth * 7;
    const counterP2 = `P2 ${
      largestBasinRating(currentBasins).toString().padStart(7)
    }`;
    const counterP2Color = isPart2Done ? COLOR_COUNTER_END : COLOR_COUNTER;
    const textP2XOffset = textXOffset + counterP1Width + margin;
    writeText(frame, counterP2, textP2XOffset, textYOffset, counterP2Color);
  }

  gif.push(frame);
};

const lowPointChunks = chunk(lowPoints, 10);

const currentLowPoints: Coord[] = [];

lowPointChunks.forEach((lowPointChunk) => {
  drawFrame();

  currentLowPoints.push(...lowPointChunk);
});

lowPoints.forEach((p, i) => currentFrontiers[i].add(p));

while (currentFrontiers.some((f) => f.size > 0)) {
  drawFrame();

  for (let i = 0; i < lowPoints.length; i++) {
    if (currentFrontiers[i].size > 0) {
      const basin = currentBasins[i];
      const newFrontier = new CoordSet();

      currentFrontiers[i].forEach((point) => {
        const neighbors = findNeighbors(input, point);

        neighbors.forEach((neighbor) => {
          const neighborValue = indexWithCoord(input, neighbor);
          if (neighborValue < 9 && !basin.has(neighbor)) {
            basin.add(neighbor);
            newFrontier.add(neighbor);
          }
        });
      });

      currentFrontiers[i] = newFrontier;
    }
  }
}

drawFrame(true);

console.timeEnd("calculation");

console.log("Encoding...");

console.time("encoding");
const bytes = await gif.encode();
console.timeEnd("encoding");

console.log("Writing...");

Deno.writeFile("output.gif", bytes);

console.log("Done!");
