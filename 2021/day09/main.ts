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

      console.log({ x, y, neighbors });

      const value = input[y][x];
      const isLowPoint = neighbors.every(([nx, ny]) => value < input[ny][nx]);
      if (isLowPoint) {
        lowPoints.push([x, y]);
      }
    }
  }
  console.log({ lowPoints });

  const lowPointValues = lowPoints.map(([x, y]) => input[y][x] + 1);

  const map = input.map((l, y) =>
    l.map((v, x) => {
      const ilp = lowPoints.some(([nx, ny]) => nx === x && ny === y);
      return ilp ? "_" : v;
    }).join("")
  ).join("\n");

  console.log(map);

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
