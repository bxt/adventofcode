#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { Coord, ensureElementOf, sum } from "../../2020/utils.ts";

const parseInput = (string: string): number[][] =>
  string.trim().split("\n").map((line) => {
    return line.trim().split("").map((s) => parseInt(s, 10));
  });

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

const part1 = (input: number[][]): number => {
  const lowPoints: Coord[] = [];
  for (let y = 0; y < input.length; y++) {
    for (let x = 0; x < input[y].length; x++) {
      const neighbors: Coord[] = [];
      if (y > 0) neighbors.push([x, y - 1]);
      if (x > 0) neighbors.push([x - 1, y]);
      if (y < input.length - 1) neighbors.push([x, y + 1]);
      if (x < input[y].length - 1) neighbors.push([x + 1, y]);

      const value = input[y][x];
      const isLowPoint = neighbors.every(([nx, ny]) => value < input[ny][nx]);
      if (isLowPoint) {
        lowPoints.push([x, y]);
      }
    }
  }
  const lowPointValues = lowPoints.map(([x, y]) => input[y][x] + 1);

  const map = input.map((l, y) =>
    l.map((v, x) => {
      const ilp = lowPoints.some(([nx, ny]) => nx === x && ny === y);
      return ilp ? "_" : v;
    }).join("")
  ).join("\n");

  return sum(lowPointValues);
};

const example = parseInput(`
  2199943210
  3987894921
  9856789892
  8767896789
  9899965678
`);

assertEquals(part1(example), 15);

console.log("Result part 1: " + part1(input));

const part2 = (input: number[][]): number => {
  const lowPoints: Coord[] = [];
  for (let y = 0; y < input.length; y++) {
    for (let x = 0; x < input[y].length; x++) {
      const neighbors: Coord[] = [];
      if (y > 0) neighbors.push([x, y - 1]);
      if (x > 0) neighbors.push([x - 1, y]);
      if (y < input.length - 1) neighbors.push([x, y + 1]);
      if (x < input[y].length - 1) neighbors.push([x + 1, y]);

      const value = input[y][x];
      const isLowPoint = neighbors.every(([nx, ny]) => value < input[ny][nx]);
      if (isLowPoint) {
        lowPoints.push([x, y]);
      }
    }
  }
  console.log({ lowPoints });

  const basins = lowPoints.map((p) => {
    const basin: Coord[] = [];
    basin.push(p);

    const addNeighbors = ([x, y]: Coord) => {
      const neighbors: Coord[] = [];
      if (y > 0) neighbors.push([x, y - 1]);
      if (x > 0) neighbors.push([x - 1, y]);
      if (y < input.length - 1) neighbors.push([x, y + 1]);
      if (x < input[y].length - 1) neighbors.push([x + 1, y]);

      for (const n of neighbors) {
        const [nx, ny] = n;
        const neighborValue = input[ny][nx];
        if (
          neighborValue < 9 && !basin.some(([ex, ey]) => ex === nx && ey === ny)
        ) {
          basin.push(n);
          addNeighbors(n);
        }
      }
    };

    addNeighbors(p);

    return basin;
  });

  console.log({ basins });

  const map = input.map((l, y) =>
    l.map((v, x) => {
      const ilp = lowPoints.some(([nx, ny]) => nx === x && ny === y);
      const ibp = !ilp &&
        basins.some((b) => b.some(([nx, ny]) => nx === x && ny === y));
      return ilp ? "_" : ibp ? "-" : v;
    }).join("")
  ).join("\n");

  console.log(map);

  const basinSizes = basins.map((b) => b.length).sort((a, b) => b - a);
  const largestSizes = basinSizes.slice(0, 3);
  console.log({ basinSizes, largestSizes });

  return largestSizes.reduce((acc, n) => n * acc, 1);
};

assertEquals(part2(example), 1134);

console.log("Result part 2: " + part2(input));
