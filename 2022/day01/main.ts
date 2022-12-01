#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { sum } from "../../2020/utils.ts";

const parseInput = (string: string): number[][] =>
  string.trim().split("\n\n").map((block) => {
    const lines = block.trim().split("\n");
    return lines.map((line) => parseInt(line.trim(), 10));
  });

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

const part1 = (input: number[][]): number => {
  const sums = input.map(sum);
  return Math.max(...sums);
};

const example = parseInput(`
  1000
  2000
  3000

  4000

  5000
  6000

  7000
  8000
  9000

  10000
`);

assertEquals(part1(example), 24000);

console.log("Result part 1: " + part1(input));

const part2 = (input: number[][]): number => {
  const sums = input.map(sum);
  const sortedSums = sums.sort((a, b) => b - a);
  const topThree = sortedSums.slice(0, 3);
  return sum(topThree);
};

assertEquals(part2(example), 45000);

console.log("Result part 2: " + part2(input));
