#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { Coord, CoordSet, indexWithCoord, sum } from "../../2020/utils.ts";

const parseInput = (string: string): number[][] =>
  string.trim().split("\n").map((line) => {
    return line.trim().split("").map((s) => parseInt(s, 10));
  });

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

const findNeighbors = (input: number[][], [x, y]: Coord) => {
  const neighbors: Coord[] = [];
  if (y > 0) neighbors.push([x, y - 1]);
  if (x > 0) neighbors.push([x - 1, y]);
  if (y < input.length - 1) neighbors.push([x, y + 1]);
  if (x < input[y].length - 1) neighbors.push([x + 1, y]);
  return neighbors;
};

const findLowPoints = (input: number[][]): Coord[] => {
  const lowPoints: Coord[] = [];
  for (let y = 0; y < input.length; y++) {
    for (let x = 0; x < input[y].length; x++) {
      const point: Coord = [x, y];
      const neighbors = findNeighbors(input, point);

      const value = indexWithCoord(input, point);
      const isLowPoint = neighbors.every((n) =>
        value < indexWithCoord(input, n)
      );
      if (isLowPoint) {
        lowPoints.push(point);
      }
    }
  }
  return lowPoints;
};

const part1 = (input: number[][]): number => {
  const lowPoints = findLowPoints(input);

  const lowPointValues = lowPoints.map((p) => indexWithCoord(input, p) + 1);

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

const product = (numbers: number[]) => numbers.reduce((acc, n) => n * acc, 1);

const part2 = (input: number[][]): number => {
  const lowPoints = findLowPoints(input);

  const basins = lowPoints.map((lowPoint) => {
    const basin = new CoordSet();
    basin.add(lowPoint);

    const addNeighbors = (point: Coord) => {
      const neighbors = findNeighbors(input, point);

      neighbors.forEach((neighbor) => {
        const neighborValue = indexWithCoord(input, neighbor);
        if (neighborValue < 9 && !basin.has(neighbor)) {
          basin.add(neighbor);
          addNeighbors(neighbor);
        }
      });
    };

    addNeighbors(lowPoint);

    return basin;
  });

  const basinSizes = basins.map((b) => b.size).sort((a, b) => b - a);
  const largestSizes = basinSizes.slice(0, 3);
  return product(largestSizes);
};

assertEquals(part2(example), 1134);

console.log("Result part 2: " + part2(input));
