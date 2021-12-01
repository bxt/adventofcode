#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { ensureElementOf } from "../utils.ts";

const TREE = "#";
const SPACE = ".";
type Entry = typeof TREE | typeof SPACE;

const parseInput = (string: string): Entry[][] =>
  string.trim().split("\n").map((line) => (
    line.split("").map((letter) =>
      ensureElementOf(letter, [TREE, SPACE] as const)
    )
  ));

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

type Vector = {
  down: number;
  right: number;
};

const START_POSITION: Vector = { down: 0, right: 0 };

const addVectors = (
  v1: Vector,
  v2: Vector,
): Vector => ({ down: v1.down + v2.down, right: v1.right + v2.right });

const walk = (treeLists: Entry[][], slope: Vector) => {
  let position = START_POSITION;
  let treeCount = 0;

  while (position.down < treeLists.length) {
    const treeRow = treeLists[position.down];
    if (treeRow[position.right % treeRow.length] === TREE) {
      treeCount++;
    }
    position = addVectors(position, slope);
  }

  return treeCount;
};

const example = parseInput(`
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
`);

const PART_1_SLOPE: Vector = { down: 1, right: 3 };

const part1 = (inputs: Entry[][]) => walk(inputs, PART_1_SLOPE);

assertEquals(part1(example), 7, "Example is wrong!");

console.log("Result part 1: " + part1(inputParsed));

const PART_2_SLOPES: Vector[] = [
  { right: 1, down: 1 },
  { right: 3, down: 1 },
  { right: 5, down: 1 },
  { right: 7, down: 1 },
  { right: 1, down: 2 },
];

const part2 = (inputs: Entry[][]) =>
  PART_2_SLOPES.map((slope) => walk(inputs, slope)).reduce((a, b) => a * b, 1);

assertEquals(part2(example), 336, "Example is wrong!");

console.log("Result part 2: " + part2(inputParsed));
