#!/usr/bin/env deno run --allow-write --allow-read --allow-net
import { slidingWindows } from "https://deno.land/std@0.116.0/collections/mod.ts";
import {
  COLOR_BLUE_1,
  COLOR_BLUE_2,
  COLOR_BLUE_3,
  COLOR_BLUE_4,
  COLOR_GREEN_3,
  COLOR_GREEN_4,
  drawNeatCircle,
  drawNeatLine,
  fontWidth,
  Frame,
  GIF,
  mixColors,
  writeText,
} from "../visualisation_utils/mod.ts";
import {
  compose,
  Coord3,
  findAlignment,
  findAlignments,
  input,
  mainhattanDistance,
  setify,
  Transform,
} from "./main.ts";

console.log("Calculating...");
console.time("calculation");

const gif = new GIF([]);

const COLOR_BACKGROUND = COLOR_BLUE_1;
const COLOR_COUNTER = COLOR_BLUE_3;
const COLOR_COUNTER_END = COLOR_GREEN_3;
const COLOR_BEACON = COLOR_BLUE_2;
const COLOR_SCANNER = COLOR_BLUE_4;
const COLOR_BEACON_ADDED = COLOR_BLUE_4;
const COLOR_BEACON_MATCHING = COLOR_GREEN_3;
const COLOR_SCANNER_MATCHING = COLOR_GREEN_4;
const COLOR_MANHATTAN = mixColors(COLOR_BLUE_2, COLOR_BACKGROUND, 0.75);
const COLOR_MANHATTAN_BEST = mixColors(COLOR_GREEN_3, COLOR_BACKGROUND, 0.6);

const margin = 20;

const width = 1280;
const height = 720;

const FRAMES_PER_STEP = 20;
const ANGLE_PER_FRAME = 1.5;
const CAM_DOWN_ANGLE = 30;
const CAM_DISTANCE = 8000;

const focalLength = CAM_DISTANCE / 16;

const CACHE_PATH = "alignments.json";

let alignments: [number, number][];
try {
  alignments = JSON.parse(await Deno.readTextFile(CACHE_PATH));
} catch (error) {
  if (error instanceof Deno.errors.NotFound) {
    console.log("Recalculating assignments...");
    ({ alignments } = findAlignments(input));
    await Deno.writeTextFile(CACHE_PATH, JSON.stringify(alignments));
  } else {
    throw error;
  }
}

const scannerPositions: Coord3[] = [[0, 0, 0]];
const allPoints: Set<string> = setify(input[0]);

let bestManhattenDistance:
  | { from: number; to: number; value: number }
  | undefined;

let frameNumber = 0;

type DrawFrameParams = {
  matchedPoints: Set<string>;
  otherAddedPoints: Set<string>;
  scannerPosition?: Coord3;
};

const drawABunchOfFrames = (
  params: DrawFrameParams,
) => {
  for (let i = 0; i < FRAMES_PER_STEP; i++) {
    drawFrame(params);
  }
};

const drawFrame = (
  {
    matchedPoints,
    otherAddedPoints,
    scannerPosition,
  }: {
    matchedPoints: Set<string>;
    otherAddedPoints: Set<string>;
    scannerPosition?: Coord3;
  },
) => {
  const isDone = scannerPosition === undefined;
  frameNumber++;

  const angle = frameNumber * ANGLE_PER_FRAME / 180 * Math.PI;
  const cameraPosition = [
    Math.sin(angle) * CAM_DISTANCE,
    CAM_DISTANCE / 2,
    Math.cos(angle) * CAM_DISTANCE,
  ];
  const cameraRotation = [-CAM_DOWN_ANGLE / 180 * Math.PI, angle, 0];

  // const camera: Transform = ([x, y, z]) => [x, y, z - 16000];
  const camera: Transform = ([px, py, pz]) => {
    const [cx, cy, cz] = cameraPosition;
    const [x, y, z] = [px - cx, py - cy, pz - cz];
    const [sinX, sinY, sinZ] = cameraRotation.map((f) => Math.sin(f));
    const [cosX, cosY, cosZ] = cameraRotation.map((f) => Math.cos(f));
    return [
      cosY * (sinZ * y + cosZ * x) - sinY * z,
      sinX * (cosY * z + sinY * (sinZ * y + cosZ * x)) +
      cosX * (cosZ * y - sinZ * x),
      cosX * (cosY * z + sinY * (sinZ * y + cosZ * x)) -
      sinX * (cosZ * y - sinZ * x),
    ];
  };

  const projection: Transform = ([x, y, z]) => [
    x / z * focalLength + width / 2,
    y / z * focalLength + height / 2,
    -z,
  ];

  const pointSize = (z: number) => 30 / z * focalLength;

  const map = compose(projection, camera);

  const frameDuration = 50;
  const frame = new Frame(width, height, frameDuration);
  frame.drawBox(1, 1, width, height, COLOR_BACKGROUND);

  const drawManhattan = (
    [x1, y1, z1]: Coord3,
    [x2, y2, z2]: Coord3,
    color: number,
  ) => {
    const steps: Coord3[] = [
      [x1, y1, z1],
      [x2, y1, z1],
      [x2, y1, z2],
      [x2, y2, z2],
    ];
    slidingWindows(steps.map(map), 2).forEach(([[x1, y1], [x2, y2]]) => {
      drawNeatLine(frame, x1, y1, x2, y2, color);
    });
  };

  if (scannerPosition) {
    const a = scannerPosition;
    for (let i = 0; i < scannerPositions.length; i++) {
      const b = scannerPositions[i];
      drawManhattan(a, b, COLOR_MANHATTAN);
    }
  }

  if (bestManhattenDistance) {
    const a = scannerPositions[bestManhattenDistance.to] ||
      scannerPosition;
    const b = scannerPositions[bestManhattenDistance.from];
    drawManhattan(a, b, COLOR_MANHATTAN_BEST);
  }

  allPoints.forEach((pointString) => {
    const point: Coord3 = JSON.parse(pointString);
    const [x, y, z] = map(point);

    const color = matchedPoints.has(pointString)
      ? COLOR_BEACON_MATCHING
      : otherAddedPoints.has(pointString)
      ? COLOR_BEACON_ADDED
      : COLOR_BEACON;

    drawNeatCircle(frame, x, y, pointSize(z), color);
  });

  scannerPositions.forEach((scannerPosition) => {
    const [x, y, z] = map(scannerPosition);
    drawNeatCircle(frame, x, y, pointSize(z) * 1.4, COLOR_SCANNER);
  });

  if (scannerPosition) {
    const [x, y, z] = map(scannerPosition);
    drawNeatCircle(frame, x, y, pointSize(z) * 1.4, COLOR_SCANNER_MATCHING);
  }

  const textYOffset = margin;
  const counterColor = isDone ? COLOR_COUNTER_END : COLOR_COUNTER;

  const counterP1Text = `P1 ${allPoints.size}`;
  const textP1XOffset = margin;
  writeText(frame, counterP1Text, textP1XOffset, textYOffset, counterColor);

  const counterP2Text = bestManhattenDistance !== undefined
    ? `P2 ${bestManhattenDistance.value.toString().padStart(4)}`
    : "";
  const textP2XOffset = width - margin - fontWidth * counterP2Text.length;
  writeText(frame, counterP2Text, textP2XOffset, textYOffset, counterColor);

  gif.push(frame);
};

const noopTransform = compose();

const alignmentQueue: [number, Transform][] = [[0, noopTransform]];

drawABunchOfFrames({
  matchedPoints: new Set(),
  otherAddedPoints: setify(input[0]),
  scannerPosition: [0, 0, 0],
});

while (alignmentQueue.length > 0) {
  const firstInQueue = alignmentQueue.shift();
  if (firstInQueue === undefined) throw new Error("Queue magically got empty");
  const [index, transformSoFar] = firstInQueue;

  const start = input[index];

  for (const [fromIndex, nextIndex] of alignments) {
    if (fromIndex !== index) continue;

    const next = input[nextIndex];
    const alignment = findAlignment(start, next);

    if (alignment === undefined) {
      throw new Error("Precomputed alignment did not actually align");
    }

    const matchedPoints = new Set<string>();
    const otherAddedPoints = new Set<string>();

    setify(alignment.aligned.map(transformSoFar)).forEach((p) => {
      if (allPoints.has(p)) {
        matchedPoints.add(p);
      } else {
        otherAddedPoints.add(p);
      }
      allPoints.add(p);
    });

    const nextTransform = compose(transformSoFar, alignment.transform);
    const scannerPosition = nextTransform([0, 0, 0]);

    for (let i = 0; i < scannerPositions.length; i++) {
      const a = scannerPositions[i];
      const b = scannerPosition;
      const distance = mainhattanDistance(a, b);
      if (!bestManhattenDistance || bestManhattenDistance.value < distance) {
        bestManhattenDistance = {
          from: i,
          to: scannerPositions.length,
          value: distance,
        };
      }
    }

    drawABunchOfFrames({
      matchedPoints,
      otherAddedPoints,
      scannerPosition,
    });

    scannerPositions.push(scannerPosition);
    alignmentQueue.push([nextIndex, nextTransform]);
  }
}

for (let i = 0; i < 3; i++) {
  drawABunchOfFrames({
    matchedPoints: new Set(),
    otherAddedPoints: new Set(),
  });
}

console.timeEnd("calculation");

console.log("Encoding...");

console.time("encoding");
const bytes = await gif.encode();
console.timeEnd("encoding");

console.log("Writing...");

Deno.writeFile("output.gif", bytes);

console.log("Done!");
