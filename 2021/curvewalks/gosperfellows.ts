#!/usr/bin/env deno run --allow-write --allow-read --allow-net
import { Coord } from "../../2020/utils.ts";

import { getCurvePoints } from "./getCurvePoints.ts";
import { mapToBox } from "./mapToBox.ts";
import { hilbert } from "./lindenmayerSystems.ts";
import { adventOfCodeColors } from "./colors.ts";
import { renderWalk } from "./renderWalk.ts";

console.log("Calculating...");
console.time("calculation");

const originalPoints = getCurvePoints(hilbert, 8);

const margin = 32;
const width = 2560;
const height = 1600;

const points: Coord[] = mapToBox(originalPoints, margin, width, height);

const frames = 10;

const colors = adventOfCodeColors(9);

const getSegmentLength = ([[x1, y1], [x2, y2]]: Coord[]) =>
  Math.sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2));

const dotSize = getSegmentLength(points) / 3;

const gif = renderWalk({
  ...colors,
  dotSize,
  frames,
  height,
  points,
  width,
});

console.timeEnd("calculation");

console.log("Encoding...");

console.time("encoding");
const bytes = await gif.encode();
console.timeEnd("encoding");

console.log("Writing...");

Deno.writeFile("gosperfellows.gif", bytes);

console.log("Done!");
