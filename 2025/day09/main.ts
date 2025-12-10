#!/usr/bin/env deno run --allow-read=input.txt
import { assertEquals } from "jsr:@std/assert@1.0.16";

const getInputPoints = async (): Promise<[number, number][]> => {
  const file = await Deno.readTextFile("input.txt");

  const lines = file.trim().split("\n");

  return lines.map((line) => {
    const [x, y] = line.split(",").map((part) => parseInt(part, 10));
    return [x, y] as const;
  });
};

const examplePoints: [number, number][] = [
  [7, 1],
  [11, 1],
  [11, 7],
  [9, 7],
  [9, 5],
  [2, 5],
  [2, 3],
  [7, 3],
];

const getLargestRectangleArea = (points: [number, number][]): number => {
  let largestArea = 0;

  for (let i = 0; i < points.length; i++) {
    const [x1, y1] = points[i];

    for (let k = i + 1; k < points.length; k++) {
      const [x2, y2] = points[k];
      const area = (Math.abs(x2 - x1) + 1) * (Math.abs(y2 - y1) + 1);
      if (area > largestArea) {
        largestArea = area;
      }
    }
  }

  return largestArea;
};

assertEquals(getLargestRectangleArea(examplePoints), 50);

console.log(`Part 1: ${getLargestRectangleArea(await getInputPoints())}`);
