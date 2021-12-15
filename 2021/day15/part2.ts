#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { minBy } from "https://deno.land/std@0.116.0/collections/mod.ts";
import {
  addCoords,
  Coord,
  manhattanNormCoord,
  minMax,
  sum,
} from "../../2020/utils.ts";

const parseInput = (string: string): number[][] => {
  return string.trim().split("\n").map((line) =>
    line.trim().split("").map((s) => parseInt(s, 10))
  );
};

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

const parseCoord = (s: string): Coord => {
  const [a, b] = s.split(",");
  return [parseInt(a, 10), parseInt(b, 10)];
};

const neighborCoords: Coord[] = [[1, 0], [0, 1], [-1, 0], [0, -1]];

const aStar = (field: number[][]) => {
  const width = field[0].length;
  const height = field.length;

  const start = "0,0";
  const predecessors: Record<string, string> = {};
  const knownDistances: Record<string, number> = {};

  const bestDistances: Record<string, number> = {}; // Hash.new { Float::INFINITY }
  bestDistances[start] = 0;

  const queue: string[] = [];
  queue.push(start);

  const handleChildren = (current: string) => {
    const currentCoord = parseCoord(current);
    const neighbours: [string, number][] = neighborCoords.map((nc) => {
      return addCoords(nc, currentCoord);
    }).filter(([x, y]) => (
      x < width * 5 && y < height * 5 && x >= 0 && y >= 0
    )).map(([x, y]) => {
      const originalValue = field[y % height][x % width];
      const adjustedValue =
        (originalValue + Math.floor(y / height) + Math.floor(x / width) - 1) %
          9 + 1;
      return [`${x},${y}`, adjustedValue];
    });

    for (const [neighbour, distance] of neighbours) {
      if (knownDistances[neighbour] === undefined) {
        const possibleDistance = knownDistances[current] + distance;
        const bestDistance = bestDistances[neighbour] ?? Infinity;
        if (possibleDistance < bestDistance) {
          predecessors[neighbour] = current;
          bestDistances[neighbour] = possibleDistance;
          if (!queue.includes(neighbour)) {
            queue.push(neighbour);
          }
        }
      }
    }
  };

  while (queue.length > 0) {
    const current = minBy(
      queue,
      (element) => bestDistances[element] ?? Infinity,
    );
    if (current === undefined) throw new Error();
    queue.splice(queue.indexOf(current), 1);
    const currentDistance = bestDistances[current];
    knownDistances[current] = currentDistance;
    handleChildren(current);
  }

  const target = `${width * 5 - 1},${height * 5 - 1}`;

  const path = [];
  let currentTarget = target;
  while (currentTarget) {
    path.push(currentTarget);
    currentTarget = predecessors[currentTarget];
  }

  for (let y = 0; y < height * 5; y++) {
    let line = "";
    for (let x = 0; x < width * 5; x++) {
      const originalValue = field[y % height][x % width];
      const adjustedValue =
        (originalValue + Math.floor(y / height) + Math.floor(x / width) - 1) %
          9 + 1;
      line += path.includes(`${x},${y}`) ? ">" : " ";
      line += adjustedValue.toString();
    }
    console.log(line);
  }

  // console.log({ knownDistances, target });

  return knownDistances[target];
};

const part1 = (input: number[][]): number => {
  // console.log({ input });
  return aStar(input);
};

const example = parseInput(`
  1163751742
  1381373672
  2136511328
  3694931569
  7463417111
  1319128137
  1359912421
  3125421639
  1293138521
  2311944581
`);

assertEquals(part1(example), 315);

console.log("Result part 1: " + part1(input));
// 2927 too high
// 2929 without heurisitc too high as well
// 2925!
