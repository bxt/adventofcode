#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { Coord, SparseCoordArray, sum } from "../../2020/utils.ts";

type Line = [Coord, Coord];
type Input = Line[];
export type LineCoverage = SparseCoordArray<number>;

export const MIN_LINE_COVERAGE = 2;

const parseCoord = (s: string): Coord => {
  const [s1, s2] = s.split(",");
  return [parseInt(s1, 10), parseInt(s2, 10)];
};

export const parseInput = (string: string): Input => {
  const lines = string.trim().split(/\s*\n\s*/);

  return lines.map((line) => {
    const [fromString, toString] = line.split(/\s*->\s*/);
    return [parseCoord(fromString), parseCoord(toString)];
  });
};

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

const orderPairBy = (index: 0 | 1) =>
  ([from, to]: Line): Line => from[index] > to[index] ? [to, from] : [from, to];

export const countSpotsAboveMinLineCoverage = (
  lineCoverage: LineCoverage,
): number => {
  return sum(
    Object.values(lineCoverage).flatMap((row) =>
      Object.values(row).filter((n) => n >= MIN_LINE_COVERAGE).length
    ),
  );
};

export const calculateCoverage = (
  input: Input,
  enableDiagonals: boolean,
  callback: (lineCoverage: LineCoverage) => void = () => {},
): LineCoverage => {
  const lineCoverage: LineCoverage = {};

  const inreaseLineCoverageAt = ([x, y]: Coord) => {
    lineCoverage[y] ??= {};
    lineCoverage[y][x] ??= 0;
    lineCoverage[y][x]++;
  };

  input.map(orderPairBy(1)).map(orderPairBy(0)).forEach(
    ([[fromX, fromY], [toX, toY]]) => {
      if (fromX === toX) {
        const x = fromX;
        for (let y = fromY; y <= toY; y++) {
          inreaseLineCoverageAt([x, y]);
        }
      } else if (fromY === toY) {
        const y = fromY;
        for (let x = fromX; x <= toX; x++) {
          inreaseLineCoverageAt([x, y]);
        }
      } else {
        if (enableDiagonals) {
          for (let x = fromX; x <= toX; x++) {
            const y = fromY + (x - fromX) * Math.sign(toY - fromY);
            inreaseLineCoverageAt([x, y]);
          }
        }
      }
      callback(lineCoverage);
    },
  );

  return lineCoverage;
};

const part1 = (input: Input): number => {
  return countSpotsAboveMinLineCoverage(calculateCoverage(input, false));
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
  return countSpotsAboveMinLineCoverage(calculateCoverage(input, true));
};

assertEquals(part2(example), 12, "Example is wrong!");

console.log("Result part 2: " + part2(input));
