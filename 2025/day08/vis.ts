#!/usr/bin/env deno run --allow-write=output.gif --allow-read --allow-net=jsr.io
import {
  COLOR_BLUE_1,
  COLOR_BLUE_2,
  COLOR_BLUE_3,
  COLOR_BLUE_4,
  COLOR_GREEN_3,
  COLOR_GREEN_4,
  drawNeatCircle,
  drawNeatLine,
  fontHeight,
  fontWidth,
  Frame,
  GIF,
  mixColors,
  writeText,
} from "../../2021/visualisation_utils/mod.ts";
import {
  connections,
  type Coord3,
  getFrequencyCounts,
  positions,
} from "./main.ts";

type Transform = (c: Coord3) => Coord3;

function compose(...ts: Transform[]): Transform {
  return (c: Coord3) => ts.reduceRight((acc, t) => t(acc), c);
}

console.log("Calculating...");
console.time("calculation");

const gif = new GIF([]);

const COLOR_BACKGROUND = COLOR_BLUE_1;
const COLOR_COUNTER = COLOR_BLUE_3;
const COLOR_COUNTER_END = COLOR_GREEN_3;
const COLOR_BEACON = COLOR_BLUE_2;
const COLOR_BEACON_ADDED = COLOR_BLUE_4;
const COLOR_BEACON_MATCHING = COLOR_GREEN_4;
const COLOR_STATS = mixColors(COLOR_BLUE_2, COLOR_BACKGROUND, 0.75);

const margin = 20;

const width = 1280;
const height = 720;

const ANGLE_PER_FRAME = 0.9;
const CAM_DOWN_ANGLE = 30;
const CAM_DISTANCE = 120000;
const CAM_CENTER = [50000, 50000, 50000];
const ROTATION_OFFSET = 150; // rotate to a good ending position for my input

const focalLength = CAM_DISTANCE / 192;

let frameNumber = 0;

type DrawFrameParams = {
  matchedPoints: Set<string>;
  otherAddedPoints: Set<string>;
  scannerPosition?: Coord3;
};

const drawFrame = () => {
  frameNumber++;

  const angle = (frameNumber - ROTATION_OFFSET) * ANGLE_PER_FRAME / 180 *
    Math.PI;
  const cameraPosition = [
    CAM_CENTER[0] + Math.sin(angle) * CAM_DISTANCE,
    CAM_CENTER[1] + CAM_DISTANCE / 2,
    CAM_CENTER[2] + Math.cos(angle) * CAM_DISTANCE,
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

  const pointSize = (z: number) => 300 / z * focalLength;

  const map = compose(projection, camera);

  const frameDuration = 50;
  const frame = new Frame(width, height, frameDuration);
  frame.drawBox(1, 1, width, height, COLOR_BACKGROUND);

  const frequencyCountsRle = Object.entries(
    Object.groupBy(frequencyCounts, (id) => id),
  ).map(
    ([size, members]) => `${size}*${members?.length || 0}`,
  ).join(" ");

  writeText(
    frame,
    frequencyCountsRle,
    margin,
    height - margin - fontHeight,
    COLOR_STATS,
  );

  const stats = `${connectionId} ${
    frequencyCounts.length.toString().padStart(4)
  }`;
  const textStatsXOffset = width - margin - fontWidth * stats.length;

  writeText(
    frame,
    stats,
    textStatsXOffset,
    height - margin - fontHeight,
    COLOR_STATS,
  );

  positions.forEach((point: Coord3) => {
    const [x, y, z] = map(point);

    drawNeatCircle(frame, x, y, pointSize(z), COLOR_BEACON_MATCHING);
  });

  for (let cId = 0; cId <= connectionId; cId++) {
    const connection = connections[cId];
    const a = positions[connection.from];
    const b = positions[connection.to];
    const [x1, y1] = map(a);
    const [x2, y2] = map(b);

    const color = cId === connectionId
      ? COLOR_BEACON_MATCHING
      : mspConnections.has(cId)
      ? COLOR_BEACON_ADDED
      : COLOR_BEACON;

    drawNeatLine(frame, x1, y1, x2, y2, color);
  }

  const part1inProgress = frequencyCounts.slice(0, 3)
    .reduce((a, b) => a * b, 1);

  const textYOffset = margin;
  const counterColorP1 = part1 === undefined
    ? COLOR_COUNTER_END
    : COLOR_COUNTER;
  const counterColorP2 = part2 === undefined
    ? COLOR_COUNTER_END
    : COLOR_COUNTER;

  const counterP1Text = `P1 ${part1 === undefined ? part1inProgress : part1}`;
  const textP1XOffset = margin;
  writeText(frame, counterP1Text, textP1XOffset, textYOffset, counterColorP1);

  const counterP2Text = part2 !== undefined
    ? `P2 ${part2.toString().padStart(4)}`
    : "";
  const textP2XOffset = width - margin - fontWidth * counterP2Text.length;
  writeText(frame, counterP2Text, textP2XOffset, textYOffset, counterColorP2);

  gif.push(frame);
};

const componentIds = positions.map((_, index) => index);
let frequencyCounts = getFrequencyCounts(componentIds);
const mspConnections = new Set<number>();
let connectionId = 0;

let part1: undefined | number = undefined;
let part2: undefined | number = undefined;

for (; connectionId < connections.length; connectionId++) {
  const { from, to } = connections[connectionId];
  const fromComponentId = componentIds[from];
  const toComponentId = componentIds[to];

  if (fromComponentId !== toComponentId) {
    mspConnections.add(connectionId);
    for (let i = 0; i < componentIds.length; i++) {
      if (componentIds[i] === toComponentId) {
        componentIds[i] = fromComponentId;
      }
    }

    drawFrame();
  } else {
    if (connectionId % 30 === 0) {
      drawFrame();
    }
  }

  frequencyCounts = getFrequencyCounts(componentIds);

  if (connectionId === 999) {
    part1 = frequencyCounts.slice(0, 3)
      .reduce((a, b) => a * b, 1);

    console.log(`Part 1: ${part1}`);
  }

  if (frequencyCounts.length === 1) {
    part2 = positions[from][0] * positions[to][0];
    console.log(`Part 2: ${part2}`);
    break;
  }
}

for (let i = 0; i < 30; i++) {
  drawFrame();
}

console.timeEnd("calculation");

console.log("Encoding...");

console.time("encoding");
const bytes = await gif.encode();
console.timeEnd("encoding");

console.log("Writing...");

Deno.writeFile("output.gif", bytes);

console.log("Done!");
