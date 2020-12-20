#!/usr/bin/env deno run --allow-read
import {
  assert,
  assertEquals,
} from "https://deno.land/std@0.79.0/testing/asserts.ts";
import { ensureElementOf, sum } from "../utils.ts";

// SUSI OFFICIALLY ENTERED THE BUILDING! https://www.twitch.tv/veloxxmusic

const ACTIVE = "#";
const INACTIVE = ".";
type Entry = typeof ACTIVE | typeof INACTIVE;

const parseInput = (string: string): Entry[][] =>
  string.trim().split("\n").map((line) => (
    line.trim().split("").map((letter) =>
      ensureElementOf(letter, [ACTIVE, INACTIVE] as const)
    )
  ));

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

type Trinion = {
  x: number;
  y: number;
  z: number;
};

const example = parseInput(`
  .#.
  ..#
  ###
`);

const offsets = [1, 0, -1];
const neighbors = offsets.flatMap((z) =>
  offsets.flatMap((y) => offsets.map((x) => ({ x, y, z })))
).filter(({ x, y, z }) => !(x === 0 && y === 0 && z === 0));

assertEquals(neighbors.length, 26);

const getValueAt = (pocketDimension: Entry[][][], trinion: Trinion): Entry => {
  const { x, y, z } = trinion;
  if (z < 0 || z >= pocketDimension.length) return INACTIVE;
  if (y < 0 || y >= pocketDimension[z].length) return INACTIVE;
  if (x < 0 || x >= pocketDimension[z][y].length) return INACTIVE;
  return pocketDimension[z][y][x];
};

const next = (pocketDimension: Entry[][][]): Entry[][][] => {
  const result: Entry[][][] = [];
  for (let z = -1; z < pocketDimension.length + 1; z++) {
    const slice: Entry[][] = [];
    for (let y = -1; y < pocketDimension[0].length + 1; y++) {
      const row: Entry[] = [];
      for (let x = -1; x < pocketDimension[0][0].length + 1; x++) {
        const activeneighbors = neighbors.map(({ x: xn, y: yn, z: zn }) => ({
          x: x + xn,
          y: y + yn,
          z: z + zn,
        })).map((t) => getValueAt(pocketDimension, t)).filter((e) =>
          e === ACTIVE
        ).length;
        const currentValue = getValueAt(pocketDimension, { x, y, z });
        const nextValue = currentValue === ACTIVE
          ? (activeneighbors === 2 || activeneighbors === 3) ? ACTIVE : INACTIVE
          : (activeneighbors === 3)
          ? ACTIVE
          : INACTIVE;
        row.push(nextValue);
      }
      slice.push(row);
    }
    result.push(slice);
  }
  return result;
};

assertEquals(
  next([example])[0],
  parseInput(`
    .....
    .....
    .#...
    ...#.
    ..#..
  `),
);

assertEquals(
  next([example])[1],
  parseInput(`
    .....
    .....
    .#.#.
    ..##.
    ..#..
  `),
);

assertEquals(
  next([example])[2],
  parseInput(`
    .....
    .....
    .#...
    ...#.
    ..#..
  `),
);

const part1 = (input: Entry[][]) => {
  const finalPocketDimension = Array(6).fill(null).reduce(
    (pocketDimension: Entry[][][], _) => next(pocketDimension),
    [input],
  );
  return sum(
    finalPocketDimension.flatMap((slice) =>
      slice.flatMap((row) => row.flatMap((e) => e === ACTIVE ? 1 : 0))
    ),
  );
};

assertEquals(part1(example), 112);

console.log("Result part 1: " + part1(inputParsed));
