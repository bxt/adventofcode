#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import {
  groupBy,
  maxWith,
  minWith,
} from "https://deno.land/std@0.116.0/collections/mod.ts";
import { Coord, SparseCoordArray, sum } from "../../2020/utils.ts";

type Input = [Coord, Coord][];

const parseCoord = (s: string): Coord => {
  const [s1, s2] = s.split(",");
  return [parseInt(s1, 10), parseInt(s2, 10)];
};

const parseInput = (string: string): Input => {
  const lines = string.trim().split(/\s*\n\s*/);

  return lines.map((line) => {
    const [fromString, toString] = line.split(/\s*->\s*/);
    return [parseCoord(fromString), parseCoord(toString)];
  });
};

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

const countTwiceCovered = (input: Input, enableDiagonals: boolean): number => {
  console.log({ input });

  const lineCoverage: SparseCoordArray<number> = {};

  input.map(([from, to]) => from[0] < to[0] ? [from, to] : [to, from]).forEach(
    ([from, to]) => {
      const [fromX, fromY] = from;
      const [toX, toY] = to;
      console.log({ fromX, fromY, toX, toY });
      if (fromX === toX) {
        console.log("is horiz");
        const x = fromX;
        for (let y = Math.min(fromY, toY); y <= Math.max(fromY, toY); y++) {
          lineCoverage[y] ||= {};
          lineCoverage[y][x] ??= 0;
          lineCoverage[y][x]++;
        }
      } else if (fromY === toY) {
        console.log("is vert");

        const y = fromY;
        for (let x = Math.min(fromX, toX); x <= Math.max(fromX, toX); x++) {
          lineCoverage[y] ||= {};
          lineCoverage[y][x] ??= 0;
          lineCoverage[y][x]++;
        }
      } else {
        console.log("is diag");
        if (enableDiagonals) {
          const delta = Math.round((toY - fromY) / Math.abs(toY - fromY));

          for (let x = fromX; x <= toX; x++) {
            const y = fromY + (x - fromX) * delta;

            console.log({ x, y, delta });
            lineCoverage[y] ||= {};
            lineCoverage[y][x] ??= 0;
            lineCoverage[y][x]++;
          }
        }
      }
    },
  );

  console.log({ lineCoverage });

  return sum(
    Object.values(lineCoverage).flatMap((y) =>
      Object.values(y).filter((n) => n >= 2).length
    ),
  );
};

const part1 = (input: Input): number => {
  return countTwiceCovered(input, false);
};

const example = parseInput(`
  0,9 -> 5,9
  8,0 -> 0,8
  9,4 -> 3,4
  2,2 -> 2,1
  7,0 -> 7,4
  6,4 -> 2,0
  0,9 -> 2,9
  3,4 -> 1,4
  0,0 -> 8,8
  5,5 -> 8,2
`);

assertEquals(part1(example), 5, "Example is wrong!");

console.log("Result part 1: " + part1(input));

const part2 = (input: Input): number => {
  return countTwiceCovered(input, true);
};

assertEquals(part2(example), 12, "Example is wrong!");

console.log("Result part 2: " + part2(input));
// 12808 wrong
