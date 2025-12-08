#!/usr/bin/env deno run --allow-write --allow-read --allow-net
import { minBy } from "jsr:@std/collections@1.1.3"
import { input, parseCoord, setupPart2, stringifyCoord } from "./main.ts";
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

console.log("Calculating...");
console.time("calculation");

const gif = new GIF([]);

const COLOR_BACKGROUND = COLOR_BLUE_1;
const COLOR_COUNTER = COLOR_BLUE_3;
const COLOR_COUNTER_END = COLOR_GREEN_3;
const COLOR_HEIGHT = COLOR_BLUE_4;
const COLOR_KNOWN = COLOR_GREEN_3;
const COLOR_FRONTIER = COLOR_GREEN_4;
const COLOR_PATH = COLOR_BLUE_4;

const FRAME_EVERY = 2000; // of 250k iterations

const margin = 20;

const { width, height, start, target, getNeighbors, getValue } = setupPart2(
  input,
);

const imageWidth = width * 5 + 2 * margin;
const imageHeight = height * 5 + 2 * margin;

const precedessors: Record<string, string> = {};

const knownDistances: Record<string, number> = {};

const bestDistances: Record<string, number> = {};
bestDistances[start] = 0;

const queue: string[] = [];
queue.push(start);

let current: string | undefined = start;

const drawFrame = (isPart2Done: boolean) => {
  const frameDuration = isPart2Done ? 3000 : 100;
  const frame = new Frame(imageWidth, imageHeight, frameDuration);
  frame.drawBox(1, 1, imageWidth, imageHeight, COLOR_BACKGROUND);

  for (let y = 0; y < height * 5; y++) {
    for (let x = 0; x < width * 5; x++) {
      const coordString = stringifyCoord([x, y]);
      const value = getValue([x, y]);

      const distance = knownDistances[coordString];
      const isInQueue = queue.includes(coordString);
      const baseColor = mixColors(COLOR_BACKGROUND, COLOR_HEIGHT, value / 9);

      const color = isInQueue
        ? COLOR_FRONTIER
        : distance !== undefined
        ? mixColors(
          COLOR_BACKGROUND,
          COLOR_KNOWN,
          (distance / 500) % 1,
        )
        : baseColor;

      frame.setPixelAt(x + margin, y + margin, color);
    }
  }

  let pathTarget = isPart2Done ? target : current;
  if (pathTarget === undefined) throw new Error("huh?");

  const distance = knownDistances[pathTarget];

  const path = [];
  while (pathTarget !== undefined) {
    path.unshift(pathTarget);
    pathTarget = precedessors[pathTarget];
  }

  for (const coordString of path) {
    const [x, y] = parseCoord(coordString);
    frame.setPixelAt(x + margin, y + margin, COLOR_PATH);
  }

  const counterText = `P2 ${distance}`;
  const textXOffset = imageWidth - margin - fontWidth * counterText.length;
  const textYOffset = Math.floor((margin - fontHeight) / 2);
  const counterColor = isPart2Done ? COLOR_COUNTER_END : COLOR_COUNTER;
  writeText(frame, counterText, textXOffset, textYOffset, counterColor);

  gif.push(frame);
};

let iteration = 0;

while (queue.length > 0) {
  iteration++;

  current = minBy(queue, (element) => bestDistances[element]);
  if (current === undefined) throw new Error("Queue got magically empty");
  queue.splice(queue.indexOf(current), 1);

  const currentDistance = bestDistances[current];
  knownDistances[current] = currentDistance;

  const neighbours = getNeighbors(current);

  for (const [neighbour, distance] of neighbours) {
    if (knownDistances[neighbour] === undefined) {
      const possibleNewBestDistance = knownDistances[current] + distance;
      const bestDistanceSoFar = bestDistances[neighbour] ?? Infinity;
      if (possibleNewBestDistance < bestDistanceSoFar) {
        bestDistances[neighbour] = possibleNewBestDistance;
        if (!queue.includes(neighbour)) {
          precedessors[neighbour] = current;
          queue.push(neighbour);
        }
      }
    }
  }

  if (iteration % FRAME_EVERY === 0) drawFrame(false);
}

console.log({ iteration });

drawFrame(true);

console.timeEnd("calculation");

console.log("Encoding...");

console.time("encoding");
const bytes = await gif.encode();
console.timeEnd("encoding");

console.log("Writing...");

Deno.writeFile("output.gif", bytes);

console.log("Done!");
