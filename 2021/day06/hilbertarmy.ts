#!/usr/bin/env deno run --allow-write --allow-read --allow-net
import { slidingWindows } from "https://deno.land/std@0.116.0/collections/mod.ts";
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import {
  COLOR_BLUE_1,
  COLOR_BLUE_4,
  COLOR_GREEN_4,
  drawNeatCircle,
  Frame,
  GIF,
  mixColors,
} from "../visualisation_utils/mod.ts";
import { Coord, range } from "../../2020/utils.ts";

// Points walking a Hilbert curve
// - One point takes more than 18h to traverse the picture.
// - No points ever cross paths.
// - No point ever visits the same spot twice, no loops.

// Inspired by Etienne Jacob, see:
// https://twitter.com/etiennejcb/status/1469381639134453764

// Basic algorithm from Wikipedia, see:
// https://de.wikipedia.org/w/index.php?title=Hilbert-Kurve&oldid=217036415#Iterativer_Algorithmus_mit_ganzzahligen_Ein-.2FAusgabewerten
const indexToCoords = (index: number, size: number): Coord => {
  let [x, y] = [0, 0];

  for (let pot = 1; pot < size; pot <<= 1) {
    const rx = 1 & (index >> 1); // left/right
    const ry = 1 & (index ^ rx);
    [x, y] = rotate(x, y, rx, ry, pot);
    x += pot * rx;
    y += pot * ry;
    index >>= 2;
  }
  return [x, y];
};

const rotate = (
  x: number,
  y: number,
  rx: number,
  ry: number,
  pot: number,
): Coord => {
  if (ry === 0) { // diagonal mirror in "lower" half
    if (rx === 1) { // "horizontal" mirror in "right" side
      x = pot - 1 - x;
      y = pot - 1 - y;
    }
    const tmp = x;
    x = y;
    y = tmp;
  }
  return [x, y];
};

const getCurvePoints = (ldSize: number): Coord[] => {
  const size = 1 << ldSize;
  const count = 1 << (ldSize << 1);
  return range(count).map((i) => indexToCoords(i, size));
};

// 23
// 14
assertEquals(getCurvePoints(1), [[0, 0], [0, 1], [1, 1], [1, 0]]);

// 569A
// 478B
// 32DC
// 01EF
assertEquals(getCurvePoints(2), [
  [0, 0], // 0
  [1, 0], // 1
  [1, 1], // 2
  [0, 1], // 3
  [0, 2], // 4
  [0, 3], // 5
  [1, 3], // 6
  [1, 2], // 7
  [2, 2], // 8
  [2, 3], // 9
  [3, 3], // A
  [3, 2], // B
  [3, 1], // C
  [2, 1], // D
  [2, 0], // E
  [3, 0], // F
]);

console.log("Calculating...");
console.time("calculation");

// ldSize no larger than 12, I'd propose!

// const ldSize = 9;
// const dotDistance = 3;
// const dotSize = 1;

const ldSize = 8;
const dotDistance = 5;
const dotSize = 2.1;

// const ldSize = 4;
// const dotDistance = 50;
// const dotSize = 17;

const size = 1 << ldSize;
const count = 1 << (ldSize << 1);

console.log({ ldSize, size, count });

const originalPoints = getCurvePoints(ldSize);

const marginX = 640;
const marginY = 80;

const width = dotDistance * size + 2 * marginX;
const height = dotDistance * size + 2 * marginY;

const extra = Math.ceil(marginY / dotDistance);
const points: Coord[] = [
  ...range(extra).map((n): Coord => [0, -n - 1]).reverse(),
  ...originalPoints,
  ...range(extra).map((n): Coord => [size - 1, -n - 1]),
];

const frames = 10;

const COLOR_BACKGROUND = COLOR_BLUE_1;

// const dotColors = [
//   COLOR_GREEN_4,
//   COLOR_GREEN_3,
//   COLOR_BLUE_3,
//   COLOR_BLUE_4,
//   0xff8800ff,
//   0xbb7700ff,
//   0x990000ff,
//   0x660000ff,
// ];

const bl = 8;
const dotColors = [
  COLOR_GREEN_4,
  ...range(bl).map((n) => mixColors(COLOR_BLUE_4, COLOR_BACKGROUND, n / bl)),
];

const gif = new GIF([]);

for (let colorIndex = 0; colorIndex < dotColors.length; colorIndex++) {
  for (const frameNumber of range(frames)) {
    const framePercentage = frameNumber / frames;
    const frame = new Frame(width, height, 100);
    frame.drawBox(1, 1, width, height, COLOR_BACKGROUND);

    const lines = slidingWindows(points, 2);
    for (let lineNumber = 0; lineNumber < lines.length; lineNumber++) {
      const half = Math.round(framePercentage);
      if ((half + lineNumber) % 2 == 0) continue;
      const linePercentage = framePercentage * 2 - half;

      const [[x1, y1], [x2, y2]] = lines[lineNumber];
      const x = linePercentage * x1 + (1 - linePercentage) * x2;
      const y = linePercentage * y1 + (1 - linePercentage) * y2;

      const color =
        dotColors[(colorIndex + Math.floor(lineNumber / 2)) % dotColors.length];

      const mappedX = x * dotDistance + marginX + Math.floor(dotDistance / 2);
      const mappedY = y * dotDistance + marginY + Math.floor(dotDistance / 2);

      drawNeatCircle(frame, mappedX, mappedY, dotSize, color);
    }

    gif.push(frame);
  }
}

console.timeEnd("calculation");

console.log("Encoding...");

console.time("encoding");
const bytes = await gif.encode();
console.timeEnd("encoding");

console.log("Writing...");

Deno.writeFile("output.gif", bytes);

console.log("Done!");

/*
 * If you look at the GIF in the browser, you can paste the following into the
 * JavaScript console to make the colors change over time:
 *
 ******

    const style = document.createElement('style');
    style.type = 'text/css';
    style.innerHTML = `
      @keyframes hue-rotate-filter-animation {
        from {
          filter: hue-rotate(0deg);
        }

        to {
          filter: hue-rotate(360deg);
        }
      }
      body {
        animation: hue-rotate-filter-animation 400s infinite;
      }
    `;
    document.getElementsByTagName('head')[0].appendChild(style);

  ******
  *
  */
