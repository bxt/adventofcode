#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { addCoords, Coord, CoordSet } from "../../2020/utils.ts";

const FLASH_AFTER = 9;

const parseInput = (string: string): number[][] =>
  string.trim().split("\n").map((line) => {
    return line.trim().split("").map((n) => parseInt(n, 10));
  });

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

const neighborDirections: Coord[] = [
  [-1, -1],
  [+0, -1],
  [+1, -1],
  [-1, +0],
  [+1, +0],
  [-1, +1],
  [+0, +1],
  [+1, +1],
];

type RunFlashesCallbackParams = { step: number; flashedCount: number };

const runFlashesUntil = <R>(
  input: number[][],
  callback: (params: RunFlashesCallbackParams) => R | undefined,
): R => {
  const state = input.map((line) => [...line]);

  const isInBounds = ([x, y]: Coord) =>
    state[y] !== undefined && state[y][x] !== undefined;

  for (let step = 1; true; step++) {
    const flashed = new CoordSet();

    const increase = (coord: Coord) => {
      if (flashed.has(coord)) return;
      const [x, y] = coord;
      state[y][x]++;
      if (state[y][x] > FLASH_AFTER) flash(coord);
    };

    const flash = (coord: Coord) => {
      flashed.add(coord);
      const [x, y] = coord;
      state[y][x] = 0;

      neighborDirections.forEach((neighborDirection) => {
        const neighbor = addCoords(neighborDirection, coord);
        if (!isInBounds(neighbor)) return;
        increase(neighbor);
      });
    };

    for (let y = 0; y < state.length; y++) {
      for (let x = 0; x < state[y].length; x++) {
        increase([x, y]);
      }
    }

    const callbackResult = callback({ step, flashedCount: flashed.size });
    if (callbackResult !== undefined) return callbackResult;
  }
};

const part1 = (input: number[][]): number => {
  let totalFlashedCount = 0;
  runFlashesUntil(input, ({ step, flashedCount }) => {
    totalFlashedCount += flashedCount;
    if (step === 100) return true;
  });
  return totalFlashedCount;
};

const example = parseInput(`
  5483143223
  2745854711
  5264556173
  6141336146
  6357385478
  4167524645
  2176841721
  6882881134
  4846848554
  5283751526
`);

assertEquals(part1(example), 1656);

console.log("Result part 1: " + part1(input));

const part2 = (input: number[][]): number => {
  const inputCount = input[0].length * input.length;
  return runFlashesUntil(input, ({ step, flashedCount }) => {
    if (flashedCount === inputCount) return step;
  });
};

assertEquals(part2(example), 195);

console.log("Result part 2: " + part2(input));
