#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import { ensureElementOf, range, sum } from "../utils.ts";

// SUZI OFFICIALLY ENTERED THE BUILDING! https://www.twitch.tv/veloxxmusic

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

type Quaternion = {
  x: number;
  y: number;
  z: number;
  w: number;
};

const example = parseInput(`
  .#.
  ..#
  ###
`);

const offsets = [1, 0, -1];
const neighbors = offsets.flatMap((w) =>
  offsets.flatMap((z) =>
    offsets.flatMap((y) => offsets.map((x) => ({ x, y, z, w })))
  )
).filter(({ x, y, z, w }) => !(x === 0 && y === 0 && z === 0 && w === 0));

assertEquals(neighbors.length, 80);

const getValueAt = (
  pocketDimension: Entry[][][][],
  quaternion: Quaternion,
): Entry => {
  const { x, y, z, w } = quaternion;
  if (w < 0 || w >= pocketDimension.length) return INACTIVE;
  if (z < 0 || z >= pocketDimension[w].length) return INACTIVE;
  if (y < 0 || y >= pocketDimension[w][z].length) return INACTIVE;
  if (x < 0 || x >= pocketDimension[w][z][y].length) return INACTIVE;
  return pocketDimension[w][z][y][x];
};

const next = (pocketDimension: Entry[][][][]): Entry[][][][] => {
  const result: Entry[][][][] = [];
  for (let w = -1; w < pocketDimension.length + 1; w++) {
    const hyperspace: Entry[][][] = [];
    for (let z = -1; z < pocketDimension[0].length + 1; z++) {
      const slice: Entry[][] = [];
      for (let y = -1; y < pocketDimension[0][0].length + 1; y++) {
        const row: Entry[] = [];
        for (let x = -1; x < pocketDimension[0][0][0].length + 1; x++) {
          const activeneighbors =
            neighbors.map(({ x: xn, y: yn, z: zn, w: wn }) => ({
              x: x + xn,
              y: y + yn,
              z: z + zn,
              w: w + wn,
            })).map((t) => getValueAt(pocketDimension, t)).filter((e) =>
              e === ACTIVE
            ).length;
          const currentValue = getValueAt(pocketDimension, { x, y, z, w });
          const nextValue = currentValue === ACTIVE
            ? (activeneighbors === 2 || activeneighbors === 3)
              ? ACTIVE
              : INACTIVE
            : (activeneighbors === 3)
            ? ACTIVE
            : INACTIVE;
          row.push(nextValue);
        }
        slice.push(row);
      }
      hyperspace.push(slice);
    }
    result.push(hyperspace);
  }
  return result;
};

assertEquals(
  next([[example]])[1][0],
  parseInput(`
    .....
    .....
    .#...
    ...#.
    ..#..
  `),
);

assertEquals(
  next([[example]])[1][1],
  parseInput(`
    .....
    .....
    .#.#.
    ..##.
    ..#..
  `),
);

assertEquals(
  next([[example]])[1][2],
  parseInput(`
    .....
    .....
    .#...
    ...#.
    ..#..
  `),
);

const part2 = (input: Entry[][]) => {
  const finalPocketDimension = range(6).reduce(next, [[input]]);
  return sum(
    finalPocketDimension.flatMap((hyperspace) =>
      hyperspace.flatMap((slice) =>
        slice.flatMap((row) => row.flatMap((e) => e === ACTIVE ? 1 : 0))
      )
    ),
  );
};

assertEquals(part2(example), 848);

console.log("Result part 2: " + part2(inputParsed));
