#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";
import { sum } from "../utils.ts";

const parseInput = (string: string): string[][][] =>
  string.trim().split("\n\n").map((block) => (
    block.trim().split(/[ \n]+/).map((line) => line.split(""))
  ));

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

const example = parseInput(`
  abc

  a
  b
  c

  ab
  ac

  a
  a
  a
  a

  b
`);

const sumCounts = (groups: string[][][]): number =>
  sum(groups.map((group) => new Set(group.flat()).size));

assertEquals(sumCounts(example), 11);

const part1 = sumCounts(inputParsed);

console.log("Result part 1: " + part1);

const intersect = <T>(...sets: Set<T>[]): Set<T> => {
  const [set1, ...otherSets] = sets;
  return new Set(
    [...set1].filter((item) => otherSets.every((set) => set.has(item))),
  );
};

assertEquals(
  intersect(new Set([1, 2, 3, 4]), new Set([1, 2, 3]), new Set([2, 3, 4])),
  new Set([2, 3]),
);

assertEquals(
  intersect(new Set([]), new Set([1, 2, 3])),
  new Set(),
);

const sumIntersectionCounts = (groups: string[][][]): number =>
  sum(
    groups.map((group) =>
      intersect(...group.map((person) => new Set(person))).size
    ),
  );

assertEquals(
  sumIntersectionCounts(example),
  6,
);

const part2 = sumIntersectionCounts(inputParsed);

console.log("Result part 2: " + part2);
