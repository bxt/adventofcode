#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import {
  Coord,
  CoordSet,
  ensureElementOf,
  minMax,
  range,
} from "../../2020/utils.ts";

const axises = ["x", "y"] as const;

type Fold = { axis: typeof axises[number]; position: number };

type Input = {
  points: Coord[];
  folds: Fold[];
};

const parseInput = (string: string): Input => {
  const [pointsString, foldsString] = string.trim().split("\n\n");

  const points = pointsString.trim().split("\n").map((line) => {
    const [a, b] = line.trim().split(",");
    const point: Coord = [parseInt(a, 10), parseInt(b, 10)];
    return point;
  });

  const folds = foldsString.trim().split("\n").map((line) => {
    const matches = line.match(/fold along ([xy])=(\d+)/);
    if (!matches) throw new Error(`Does not match: "${line}"`);
    return {
      axis: ensureElementOf(matches[1], axises),
      position: parseInt(matches[2], 10),
    };
  });

  return { points, folds };
};

const doFold = (pointsSet: CoordSet, { axis, position }: Fold) => {
  const nextPointSet = new CoordSet();

  if (axis === "y") {
    for (const [x, y] of pointsSet) {
      if (y > position) {
        nextPointSet.add([x, position - (y - position)]);
      } else {
        nextPointSet.add([x, y]);
      }
    }
  } else if (axis === "x") {
    for (const [x, y] of pointsSet) {
      if (x > position) {
        nextPointSet.add([position - (x - position), y]);
      } else {
        nextPointSet.add([x, y]);
      }
    }
  }

  return nextPointSet;
};

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

const part1 = (input: Input): number => {
  const fold = input.folds[0];
  const pointsSet = doFold(new CoordSet(input.points), fold);
  return pointsSet.size;
};

const example = parseInput(`
  6,10
  0,14
  9,10
  0,3
  10,4
  4,11
  6,0
  6,12
  4,1
  0,13
  10,12
  3,4
  3,0
  8,4
  1,10
  2,14
  8,10
  9,0

  fold along y=7
  fold along x=5
`);

assertEquals(part1(example), 17);

console.log("Result part 1: " + part1(input));

const part2 = (input: Input): string => {
  const pointsSet = input.folds.reduce(doFold, new CoordSet(input.points));

  const [[minX, maxX], [minY, maxY]] = range(2).map((i) =>
    minMax([...pointsSet].map((p) => p[i]))
  );

  return range(maxY - minY + 1).map((yOffset) =>
    range(maxX - minX + 1).map((xOffset) => {
      const point: Coord = [xOffset + minX, yOffset + minY];
      return pointsSet.has(point) ? "#" : " ";
    }).join("")
  ).join("\n");
};

console.log("Example part 2:\n" + part2(example));

console.log("Result part 2:\n" + part2(input));
