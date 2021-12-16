#!/usr/bin/env deno run --allow-write --allow-read --allow-net
import { Coord } from "./deps.ts";

import { getCurvePoints } from "./getCurvePoints.ts";
import { mapToBox } from "./mapToBox.ts";
import * as curves from "./lindenmayerSystems.ts";
import { adventOfCodeColors } from "./colors.ts";
import { renderWalk } from "./renderWalk.ts";
import { handleFlags, HandleFlagsResult } from "./handleFlags.ts";

const processFlags = (flags: HandleFlagsResult) => {
  switch (flags.result) {
    case "help":
      console.log(flags.helpText);
      Deno.exit();
      throw new Error("Unreachable");
    case "error":
      console.error(`Error, invalid flags: ${flags.error}.`);
      Deno.exit(1);
      throw new Error("Unreachable");
    case "success":
      return flags;
  }
};

const {
  curve,
  dotFraction,
  drawLine,
  height,
  margin,
  recursion,
  width,
} = processFlags(handleFlags(Deno.args));

console.log("Calculating...");
console.time("calculation");

const originalPoints = getCurvePoints(curves[curve], recursion);

const points: Coord[] = mapToBox(originalPoints, margin, width, height);

const frames = 10;

const colors = adventOfCodeColors(9);

function getSegmentLength([[x1, y1], [x2, y2]]: Coord[]) {
  return Math.sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2));
}

const dotSize = getSegmentLength(points) / dotFraction;

const gif = renderWalk({
  ...colors,
  dotSize,
  drawLine,
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
