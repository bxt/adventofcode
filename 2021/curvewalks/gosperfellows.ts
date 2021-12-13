#!/usr/bin/env deno run --allow-write --allow-read --allow-net
import {
  minBy,
  slidingWindows,
} from "https://deno.land/std@0.116.0/collections/mod.ts";
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import {
  COLOR_BLUE_1,
  COLOR_BLUE_2,
  COLOR_BLUE_4,
  COLOR_GREEN_4,
  drawNeatCircle,
  drawNeatLine,
  Frame,
  GIF,
  mixColors,
} from "../visualisation_utils/mod.ts";
import {
  addCoords,
  Coord,
  minMax,
  range,
  scaleCoord,
} from "../../2020/utils.ts";

// Points walking a Gosper curve

// See also: http://paulbourke.net/fractals/lsys/

type LSystem = {
  start: string;
  productions: Record<string, string>;
  angle: number;
};

const gosperSystem: LSystem = {
  start: "A",
  productions: { A: "A-B--B+A++AA+B-", B: "+A-BB--B-A++A+B" },
  angle: Math.PI / 3,
};

const boxySystem: LSystem = {
  start: "F+F+F+F", // a suqare
  productions: { F: "F+F-F-FF+F+F-F" },
  angle: Math.PI / 2,
};

// The lower case letters are just for replacement and are filtered out then
const peanoSystem: LSystem = {
  start: "a",
  productions: {
    a: "aFbFa+F+bFaFb-F-aFbFa",
    b: "bFaFb-F-aFbFa+F+bFaFb",
  },
  angle: Math.PI / 2,
};

const hilbertSystem: LSystem = {
  start: "a",
  productions: {
    a: "-bF+aFa+Fb-",
    b: "+aF-bFb-Fa+",
  },
  angle: Math.PI / 2,
};

const kochFlöckleinFeinröcklein: LSystem = {
  start: "F--F--F", // equilateral triangle
  productions: { F: "F+F--F+F" },
  angle: Math.PI / 3,
};

const sierpinskiSystem: LSystem = {
  start: "F+F+F+F+F+F",
  productions: { F: "F-F+F+F+F-F" },
  angle: Math.PI / 3,
};

const sierpinskiArrowhead: LSystem = {
  start: "aF",
  productions: {
    a: "bF-aF-b",
    b: "aF+bF+a",
  },
  angle: Math.PI / 3,
};

const getSymbols = ({ start, productions }: LSystem, stages: number) => {
  let symbols = start;
  range(stages).forEach(() => {
    symbols = symbols.replaceAll(/./g, (match) => productions[match] ?? match);
  });
  return symbols;
};

assertEquals(getSymbols(gosperSystem, 0), "A");
assertEquals(getSymbols(gosperSystem, 1), "A-B--B+A++AA+B-");
assertEquals(
  getSymbols(gosperSystem, 2),
  "A-B--B+A++AA+B--+A-BB--B-A++A+B--+A-BB--B-A++A+B+A-B--B+A++AA+B-++A-B--B+A++AA+B-A-B--B+A++AA+B-++A-BB--B-A++A+B-",
);

assertEquals(getSymbols(kochFlöckleinFeinröcklein, 0), "F--F--F");
assertEquals(
  getSymbols(kochFlöckleinFeinröcklein, 1),
  "F+F--F+F--F+F--F+F--F+F--F+F",
);

const rotateCoord = ([x, y]: Coord, angle: number): Coord => [
  x * Math.cos(angle) - y * Math.sin(angle),
  x * Math.sin(angle) + y * Math.cos(angle),
];

type Instruction =
  | { type: "forward"; length: number }
  | { type: "rotate"; angle: number };

const mapSymbols = (symbols: string, angle: number): Instruction[] => {
  return [...symbols.matchAll(/-|\+|[A-Z]/g)].map(([match]) => {
    if (match === "-") {
      return { type: "rotate", angle: -angle };
    } else if (match === "+") {
      return { type: "rotate", angle };
    } else {
      return { type: "forward", length: 1 };
    }
  });
};

assertEquals(mapSymbols("ABBA+++AA", Math.PI / 3).length, 9);

const getLSystemInstructions = (lSystem: LSystem, stages: number) => {
  const symbols = getSymbols(lSystem, stages);
  return mapSymbols(symbols, lSystem.angle);
};

const walkInstructions = (instructions: Instruction[]): Coord[] => {
  let positon: Coord = [0, 0];
  let direction: Coord = [1, 0];
  const points = [positon];
  for (const instruction of instructions) {
    if (instruction.type === "rotate") {
      direction = rotateCoord(direction, instruction.angle);
    } else if (instruction.type === "forward") {
      positon = addCoords(positon, scaleCoord(direction, instruction.length));
      points.push(positon);
    }
  }
  return points;
};

assertEquals(walkInstructions(mapSymbols("", 0)), [[0, 0]]);
assertEquals(walkInstructions(mapSymbols("A", 0)), [[0, 0], [1, 0]]);
assertEquals(walkInstructions(mapSymbols("AB", 0)), [[0, 0], [1, 0], [2, 0]]);
assertEquals(walkInstructions([{ type: "forward", length: 4 }]), [[0, 0], [
  4,
  0,
]]);
assertEquals(
  walkInstructions([{ type: "forward", length: 42 }, {
    type: "forward",
    length: 1337,
  }]),
  [[0, 0], [42, 0], [42 + 1337, 0]],
);
{
  const path = walkInstructions(mapSymbols("ABBA+++AA", Math.PI / 3));
  assertEquals(path.slice(0, 5), [[0, 0], [1, 0], [2, 0], [3, 0], [4, 0]]);
  assertEquals(path[5][0], 3);
  if (Math.abs(path[5][1]) > 0.00001) throw new Error();
  assertEquals(path[6][0], 2);
  if (Math.abs(path[6][1]) > 0.00001) throw new Error();
}

const getCurvePoints = (stages: number): Coord[] => {
  return walkInstructions(
    getLSystemInstructions(sierpinskiArrowhead, stages),
  );
};

const mapToBox = (
  points: Coord[],
  margin: number,
  width: number,
  height: number,
): Coord[] => {
  const usableWidth = (width - 2 * margin);
  const usableHeight = (height - 2 * margin);
  const [minX, maxX] = minMax(points.map(([x, _]) => x));
  const [minY, maxY] = minMax(points.map(([_, y]) => y));
  const diffX = maxX - minX;
  const diffY = maxY - minY;
  const factor = Math.min(
    usableWidth / diffX,
    usableHeight / diffY,
  );
  if (factor === undefined) throw new Error();
  return points.map(([x, y]) => [
    (x - minX - diffX / 2) * factor + margin + usableWidth / 2,
    (y - minY - diffY / 2) * factor + margin + usableHeight / 2,
  ]);
};

assertEquals(mapToBox([[0, 0], [1, 1]], 10, 120, 120), [[10, 10], [110, 110]]);
assertEquals(mapToBox([[0, 0], [2, 1]], 10, 220, 120), [[10, 10], [210, 110]]);
assertEquals(mapToBox([[0, 0], [2, 1]], 10, 120, 120), [[10, 35], [110, 85]]);
assertEquals(mapToBox([[0, 0], [1, 2]], 10, 120, 120), [[35, 10], [85, 110]]);

assertEquals(mapToBox([[3, 5], [4, 6]], 10, 120, 120), [[10, 10], [110, 110]]);
assertEquals(mapToBox([[3, 5], [5, 6]], 10, 220, 120), [[10, 10], [210, 110]]);
assertEquals(mapToBox([[3, 5], [5, 6]], 10, 120, 120), [[10, 35], [110, 85]]);
assertEquals(mapToBox([[3, 5], [4, 7]], 10, 120, 120), [[35, 10], [85, 110]]);

assertEquals(mapToBox([[-3, -1], [-2, 0]], 4, 40, 40), [[4, 4], [36, 36]]);
assertEquals(mapToBox([[-3, -1], [-1, 0]], 4, 72, 40), [[4, 4], [68, 36]]);
assertEquals(mapToBox([[-3, -1], [-1, 0]], 4, 40, 40), [[4, 12], [36, 28]]);
assertEquals(mapToBox([[-3, -1], [-2, 1]], 4, 40, 40), [[12, 4], [28, 36]]);

console.log("Calculating...");
console.time("calculation");

const originalPoints = getCurvePoints(5);

const margin = 100;
const width = 1280;
const height = 960;

const points: Coord[] = mapToBox(originalPoints, margin, width, height);

const frames = 10;

const COLOR_BACKGROUND = COLOR_BLUE_1;

const getSegmentLength = ([[x1, y1], [x2, y2]]: Coord[]) =>
  Math.sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2));

const dotSize = getSegmentLength(points) / 3;

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

const bl = 7;
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

    lines.forEach(([[x1, y1], [x2, y2]]) =>
      drawNeatLine(frame, x1, y1, x2, y2, COLOR_BLUE_2)
    );

    for (let lineNumber = 0; lineNumber < lines.length; lineNumber++) {
      const half = Math.round(framePercentage);
      if ((half + lineNumber) % 2 == 0) continue;
      const linePercentage = framePercentage * 2 - half;

      const [[x1, y1], [x2, y2]] = lines[lineNumber];
      const x = linePercentage * x1 + (1 - linePercentage) * x2;
      const y = linePercentage * y1 + (1 - linePercentage) * y2;

      const color =
        dotColors[(colorIndex + Math.floor(lineNumber / 2)) % dotColors.length];

      drawNeatCircle(frame, x, y, dotSize, color);
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

Deno.writeFile("gosperfellows.gif", bytes);

console.log("Done!");
