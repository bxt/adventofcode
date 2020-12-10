#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";

const product = (numbers: number[]) =>
  numbers.reduce((acc, number) => acc * number, 1);

assertEquals(product([2, 3]), 6);
assertEquals(product([7, 6, 9]), 378);

const parseInput = (string: string): number[] =>
  string.trim().split(/[^0-9]+/)
    .map((entry) => Number(entry));

const example = parseInput(`
  16
  10
  15
  5
  1
  11
  7
  19
  6
  12
  4
`);

assertEquals(example[0], 16);
assertEquals(example[1], 10);
assertEquals(example[10], 4);
assertEquals(example.length, 11);

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

const calculateJoltDiffs = (jolts: number[]): number[] => {
  const joltsSorted = [...jolts].sort((a, b) => a - b);
  const joltsWithOutletAndDevice = [
    0,
    ...joltsSorted,
    joltsSorted[joltsSorted.length - 1] + 3,
  ];

  const diffs = [];
  for (let i = 1; i < joltsWithOutletAndDevice.length; i++) {
    const diff = joltsWithOutletAndDevice[i] - joltsWithOutletAndDevice[i - 1];
    diffs.push(diff);
  }

  return diffs;
};

const countJoltDiffs = (jolts: number[]): { diff1: number; diff3: number } => {
  const joltDiffs = calculateJoltDiffs(jolts);

  const diff1 = joltDiffs.filter((d) => d === 1).length;
  const diff3 = joltDiffs.filter((d) => d === 3).length;

  return { diff1, diff3 };
};

assertEquals(countJoltDiffs(example), { diff1: 7, diff3: 5 });

const part1 = (jolts: number[]): number => {
  const { diff1, diff3 } = countJoltDiffs(jolts);
  return diff1 * diff3;
};

assertEquals(part1(example), 35);

console.log("Result part 1: " + part1(inputParsed));

const part2 = (jolts: number[]): number => {
  const joltDiffs = calculateJoltDiffs(jolts);

  if (joltDiffs.indexOf(2) !== -1) throw new Error("Not implmented!");

  const lengths = joltDiffs.map((n) => String(n)).join("").split(/3+/).map(
    (part) => part.length,
  );

  const factors = lengths.filter((l) => l > 0).map((l) =>
    Math.pow(2, l - 1) - (l > 3 ? Math.pow(2, l - 3) - 1 : 0)
  );

  return product(factors);
};

assertEquals(part2(example), 8);

console.log(part2(inputParsed));
