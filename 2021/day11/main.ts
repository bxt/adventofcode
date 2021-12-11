#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { addCoords, Coord, CoordSet } from "../../2020/utils.ts";

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

const countFlashes = (input: number[][], iterations: number): number => {
  const state = input.map((line) => line.map((n) => n));

  const isInBounds = ([x, y]: Coord) =>
    state[y] !== undefined && state[y][x] !== undefined;

  let totalFlashCount = 0;
  for (let i = 0; i < iterations; i++) {
    const flashed = new CoordSet();

    const increase = (c: Coord) => {
      if (flashed.has(c)) return;
      const [x, y] = c;
      state[y][x]++;
      if (state[y][x] > 9) flash(c);
    };

    const flash = (c: Coord) => {
      if (flashed.has(c)) return;
      flashed.add(c);
      const [x, y] = c;
      state[y][x] = 0;

      neighborDirections.forEach((nd) => {
        const n = addCoords(nd, c);
        if (!isInBounds(n)) return;
        increase(n);
      });
    };

    for (let y = 0; y < state.length; y++) {
      for (let x = 0; x < state[y].length; x++) {
        increase([x, y]);
      }
    }
    console.log({ i, f: flashed.size });
    totalFlashCount += flashed.size;
  }
  return totalFlashCount;
};

const part1 = (input: number[][]): number => {
  return countFlashes(input, 100);
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

assertEquals(countFlashes(example, 10), 204);
assertEquals(part1(example), 1656);

console.log("Result part 1: " + part1(input));

// assertEquals(part2(example), 288957);

// console.log("Result part 2: " + part2(input));
