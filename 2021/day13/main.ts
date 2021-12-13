#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
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

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

const part1 = (input: Input): number => {
  const fold = input.folds[0];

  const pointsSet = new CoordSet();
  if (fold.axis === "y") {
    for (const [x, y] of input.points) {
      if (y > fold.position) {
        pointsSet.add([x, fold.position - (y - fold.position)]);
      } else {
        pointsSet.add([x, y]);
      }
    }
  } else if (fold.axis === "x") {
    for (const [x, y] of input.points) {
      if (x > fold.position) {
        pointsSet.add([fold.position - (x - fold.position), y]);
      } else {
        pointsSet.add([x, y]);
      }
    }
  }

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

const part2 = (input: Input): number => {
  let pointsSet = new CoordSet(input.points);

  for (const fold of input.folds) {
    const nextPointSet = new CoordSet();
    if (fold.axis === "y") {
      for (const [x, y] of pointsSet) {
        if (y > fold.position) {
          nextPointSet.add([x, fold.position - (y - fold.position)]);
        } else {
          nextPointSet.add([x, y]);
        }
      }
    } else if (fold.axis === "x") {
      for (const [x, y] of pointsSet) {
        if (x > fold.position) {
          nextPointSet.add([fold.position - (x - fold.position), y]);
        } else {
          nextPointSet.add([x, y]);
        }
      }
    }
    pointsSet = nextPointSet;
  }

  const [minX, maxX] = minMax([...pointsSet].map((p) => p[0]));
  const [minY, maxY] = minMax([...pointsSet].map((p) => p[1]));

  console.log(
    range(maxY - minY + 1).map((yOff) =>
      range(maxX - minX + 1).map((xOff) =>
        pointsSet.has([xOff + minX, yOff + minY]) ? "#" : " "
      ).join("")
    ).join("\n"),
  );

  return pointsSet.size;
};

part2(example);
console.log("Result part 2: " + part2(input));
