#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import { mapValues } from "jsr:@std/collections@1.1.3"
import { Coord, matchGroups, range } from "../../2020/utils.ts";

type Input = { x1: number; x2: number; y1: number; y2: number };

const SAFETY_PAD_JUST_IN_CASE = 3;
const ARBITRARY_FACTOR_I_MADE_UP = 4;

const matchInput = matchGroups(
  /target area: x=(?<x1>-?\d+)..(?<x2>-?\d+), y=(?<y1>-?\d+)..(?<y2>-?\d+)/,
);

function parseInput(string: string): Input {
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
}

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
    yPositions.push(y);
    if (x >= x1 && x <= x2 && y >= y1 && y <= y2) {
      return Math.max(...yPositions);
    }
    if (x > x2 || y < y1) return undefined; // miss
  }
}

function findHighPonts(input: Input): number[] {
  const possibleX = range(input.x2 + SAFETY_PAD_JUST_IN_CASE);
  const possibleY = range(input.x2 * ARBITRARY_FACTOR_I_MADE_UP).map((y) =>
    y + input.y1 - SAFETY_PAD_JUST_IN_CASE
  );
  const possibleVelocities: Coord[] = possibleX.flatMap((x) =>
    possibleY.map((y) => [x, y])
  );

  const highPoints = possibleVelocities.map((c) => findHighestPoint(c, input))
    .filter((n) => n !== undefined) as number[];

  return highPoints;
}

function part1(input: Input): number {
  return Math.max(...findHighPonts(input));
}

const example = parseInput(`
  target area: x=20..30, y=-10..-5
`);

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
  return findHighPonts(input).length;
}

assertEquals(part2(example), 112);

console.log("Result part 2: " + part2(input));
