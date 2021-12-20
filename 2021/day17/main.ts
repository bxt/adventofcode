#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { mapValues } from "https://deno.land/std@0.116.0/collections/mod.ts";
import { Coord, matchGroups, range } from "../../2020/utils.ts";

type Input = { x1: number; x2: number; y1: number; y2: number };

const matchInput = matchGroups(
  /target area: x=(?<x1>-?\d+)..(?<x2>-?\d+), y=(?<y1>-?\d+)..(?<y2>-?\d+)/,
);

const parseInput = (string: string): Input => {
  const { x1, x2, y1, y2 } = mapValues(
    matchInput(string),
    (s) => parseInt(s, 10),
  );
  return {
    x1: Math.min(x1, x2),
    x2: Math.max(x1, x2),
    y1: Math.min(y1, y2),
    y2: Math.max(y1, y2),
  };
};

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

type Probe = { position: Coord; velocity: Coord };

function step({ position: [x, y], velocity: [vx, vy] }: Probe): Probe {
  const position: Coord = [x + vx, y + vy];
  const velocity: Coord = [vx === 0 ? 0 : vx < 0 ? vx + 1 : vx - 1, vy - 1];

  return { position, velocity };
}

assertEquals(step({ position: [0, 0], velocity: [7, 2] }), {
  position: [7, 2],
  velocity: [6, 1],
});

const initialPosition: Coord = [0, 0];

function findHighestPoint(
  initialVelocity: Coord,
  targetArea: Input,
): number | undefined {
  const { x1, x2, y1, y2 } = targetArea;

  const yPositions = [];

  let probe: Probe = { position: initialPosition, velocity: initialVelocity };

  while (true) {
    probe = step(probe);
    const { position: [x, y] } = probe;
    // console.log([x, y]);
    yPositions.push(y);
    if (x >= x1 && x <= x2 && y >= y1 && y <= y2) {
      return Math.max(...yPositions);
    }
    if (x > x2 || y < y1) return undefined;
  }
}

function part1(input: Input): number {
  // console.log({ input });

  const possibleX = range(input.x2);
  const possibleY = range(input.x2 * 40);

  const highPoints = possibleX.flatMap((x) =>
    possibleY.map((y) => findHighestPoint([x, y], input))
  );

  return Math.max(...highPoints.filter((n) => n !== undefined) as number[]);
}

const example = parseInput(`
  target area: x=20..30, y=-10..-5
`);

// console.log({ example });

assertEquals(findHighestPoint([7, 2], example), 3);
assertEquals(findHighestPoint([6, 3], example), 6);
assertEquals(findHighestPoint([9, 0], example), 0);
assertEquals(findHighestPoint([17, -4], example), undefined);
assertEquals(findHighestPoint([6, 9], example), 45);
assertEquals(findHighestPoint([6, 0], example), 0);
assertEquals(findHighestPoint([7, 1], example), 1);

assertEquals(part1(example), 45);

console.log("Result part 1: " + part1(input));

function part2(input: Input): number {
  console.log({ input });

  const possibleX = range(input.x2 + 3);
  const possibleY = range(input.x2 * 40).map((y) => y + input.y1 - 3);

  const possibleVelocities: Coord[] = possibleX.flatMap((x) =>
    possibleY.map((y) => [x, y])
  );
  const hittingVelocities = possibleVelocities.filter((c) =>
    findHighestPoint(c, input) !== undefined
  );

  // console.log(hittingVelocities.map(([x, y]) => `${x},${y}`).join("\n"));

  return hittingVelocities.length;
}

assertEquals(part2(example), 112);

console.log("Result part 2: " + part2(input));
