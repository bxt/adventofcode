#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";

const sum = (numbers: number[]) =>
  numbers.reduce((acc, number) => acc + number, 0);

type Cell = "L" | "#" | ".";

const parseInput = (string: string): Cell[][] =>
  string.trim().split(/[\n ]+/)
    .map((line) =>
      line.split("").map((l) => l === "L" ? "L" : l === "#" ? "#" : ".")
    );

const example = parseInput(`
  L.LL.LL.LL
  LLLLLLL.LL
  L.L.L..L..
  LLLL.LL.LL
  L.LL.LL.LL
  L.LLLLL.LL
  ..L.L.....
  LLLLLLLLLL
  L.LLLLLL.L
  L.LLLLL.LL
`);

assertEquals(example[0][0], "L");
assertEquals(example[0][1], ".");
assertEquals(example.length, 10);
assertEquals(example[0].length, 10);

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

const neighbors = (x: number, y: number): [number, number][] => [
  [x - 1, y - 1],
  [x + 0, y - 1],
  [x + 1, y - 1],
  [x - 1, y + 0],
  [x + 1, y + 0],
  [x - 1, y + 1],
  [x + 0, y + 1],
  [x + 1, y + 1],
];

const getNeighborValues = (cells: Cell[][], x: number, y: number) =>
  neighbors(x, y).filter(([x, y]) =>
    y >= 0 && y < cells.length && x >= 0 && x < cells[y].length
  ).map(([x, y]) => cells[y][x]);

const iterateOnce = (cells: Cell[][]): [Cell[][], boolean] => {
  let changed = false;
  const newCells = cells.map((line, y) =>
    line.map((cell, x) => {
      const neighborValues = getNeighborValues(cells, x, y);
      const occupiedCount = neighborValues.filter((c) => c === "#").length;

      if (cell === "L" && occupiedCount === 0) {
        changed = true;
        return "#";
      }
      if (cell === "#" && occupiedCount >= 4) {
        changed = true;
        return "L";
      }

      return cell;
    })
  );

  return [newCells, changed];
};

const part1 = (cells: Cell[][]) => {
  let changed = true;
  while (changed) {
    const [newCells, newChanged] = iterateOnce(cells);
    cells = newCells;
    changed = newChanged;
  }
  return sum(cells.map((line) => line.filter((c) => c === "#").length));
};

assertEquals(part1(example), 37);

console.log("Result part 1: " + part1(inputParsed));
