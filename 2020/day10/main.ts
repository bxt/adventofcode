#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";

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

// const example = parseInput(`
//   1
//   4
//   5
// `);

assertEquals(example[0], 16);
assertEquals(example[1], 10);
assertEquals(example[10], 4);
assertEquals(example.length, 11);

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

const buildJoltList = (jolts: number[]): number[] => {
  const joltsSorted = [...jolts].sort((a, b) => a - b);
  const joltsWithOutletAndDevice = [
    0,
    ...joltsSorted,
    joltsSorted[joltsSorted.length - 1] + 3,
  ];
  return joltsWithOutletAndDevice;
};

const joltDiffCounts = (jolts: number[]): { diff1: number; diff3: number } => {
  const joltList = buildJoltList(jolts);

  let diff1 = 0;
  let diff3 = 0;
  for (let i = 1; i < joltList.length; i++) {
    const diff = joltList[i] - joltList[i - 1];
    if (diff > 3) throw new Error("something is fishy!");
    if (diff === 1) diff1++;
    if (diff === 3) diff3++;
  }

  return { diff1, diff3 };
};

assertEquals(joltDiffCounts(example), { diff1: 7, diff3: 5 });

const part1 = (jolts: number[]): number => {
  const { diff1, diff3 } = joltDiffCounts(jolts);
  return diff1 * diff3;
};

assertEquals(part1(example), 35);

console.log("Result part 1: " + part1(inputParsed));
