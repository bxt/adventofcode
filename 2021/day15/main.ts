#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import { minBy } from "https://deno.land/std@0.116.0/collections/mod.ts";
import { addCoords, Coord } from "../../2020/utils.ts";

const parseInput = (string: string): number[][] => {
  return string.trim().split("\n").map((line) =>
    line.trim().split("").map((s) => parseInt(s, 10))
  );
};

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

const dijkstra = (
  start: string,
  getNeighbors: (element: string) => [string, number][],
): Record<string, number> => {
  const knownDistances: Record<string, number> = {};

  const bestDistances: Record<string, number> = {};
  bestDistances[start] = 0;

  const queue: string[] = [];
  queue.push(start);

  while (queue.length > 0) {
    const current = minBy(queue, (element) => bestDistances[element]);
    if (current === undefined) throw new Error("Queue got magically empty");
    queue.splice(queue.indexOf(current), 1);

    const currentDistance = bestDistances[current];
    knownDistances[current] = currentDistance;

    const neighbours = getNeighbors(current);

    for (const [neighbour, distance] of neighbours) {
      if (knownDistances[neighbour] === undefined) {
        const possibleNewBestDistance = knownDistances[current] + distance;
        const bestDistanceSoFar = bestDistances[neighbour] ?? Infinity;
        if (possibleNewBestDistance < bestDistanceSoFar) {
          bestDistances[neighbour] = possibleNewBestDistance;
          if (!queue.includes(neighbour)) {
            queue.push(neighbour);
          }
        }
      }
    }
  }

  return knownDistances;
};

export const parseCoord = (s: string): Coord => {
  const [a, b] = s.split(",");
  return [parseInt(a, 10), parseInt(b, 10)];
};
export const stringifyCoord = ([x, y]: Coord): string => {
  return `${x},${y}`;
};

export const neighborCoords: Coord[] = [[1, 0], [0, 1], [-1, 0], [0, -1]];

const part1 = (input: number[][]): number => {
  const width = input[0].length;
  const height = input.length;
  const start = stringifyCoord([0, 0]);
  const target = stringifyCoord([width - 1, height - 1]);

  const getNeighbors = (current: string) => {
    const currentCoord = parseCoord(current);
    return neighborCoords.map((nc) => {
      return addCoords(nc, currentCoord);
    }).filter(([x, y]) => (
      x < width && y < height && x >= 0 && y >= 0
    )).map(([x, y]) => {
      return [stringifyCoord([x, y]), input[y][x]] as [string, number];
    });
  };

  return dijkstra(start, getNeighbors)[target];
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

assertEquals(part1(example), 40);

console.log("Result part 1: " + part1(input));

export const setupPart2 = (input: number[][]) => {
  const width = input[0].length;
  const height = input.length;
  const start = stringifyCoord([0, 0]);
  const target = stringifyCoord([width * 5 - 1, height * 5 - 1]);

  const getValue = ([x, y]: Coord): number => {
    const originalValue = input[y % height][x % width];
    const adjustedValue =
      (originalValue + Math.floor(y / height) + Math.floor(x / width) - 1) %
        9 + 1;
    return adjustedValue;
  };

  const getNeighbors = (current: string) => {
    const currentCoord = parseCoord(current);
    return neighborCoords.map((nc) => {
      return addCoords(nc, currentCoord);
    }).filter(([x, y]) => (
      x < width * 5 && y < height * 5 && x >= 0 && y >= 0
    )).map((coord) =>
      [stringifyCoord(coord), getValue(coord)] as [string, number]
    );
  };

  return { width, height, start, target, getNeighbors, getValue };
};

const part2 = (input: number[][]): number => {
  const { start, getNeighbors, target } = setupPart2(input);

  return dijkstra(start, getNeighbors)[target];
};

assertEquals(part2(example), 315);

console.log("Result part 2: " + part2(input));
