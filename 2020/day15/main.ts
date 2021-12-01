#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";

const example = [0, 3, 6];

const input = [0, 14, 6, 20, 1, 4];

const lastSpokenAfter = (startingNumbers: number[], index: number): number => {
  const lastSpokenAt = Object.fromEntries(
    startingNumbers.slice(0, -1).map((n, i) => [n, i + 1]),
  );
  let lastSpoken = startingNumbers[startingNumbers.length - 1];

  for (let i = startingNumbers.length + 1; i <= index; i++) {
    const nextSpoken = lastSpokenAt[lastSpoken] === undefined
      ? 0
      : i - lastSpokenAt[lastSpoken] - 1;
    lastSpokenAt[String(lastSpoken)] = i - 1;
    lastSpoken = nextSpoken;
  }

  return lastSpoken;
};

const part1 = (startingNumbers: number[]): number =>
  lastSpokenAfter(startingNumbers, 2020);

assertEquals(part1(example), 436);
assertEquals(part1([1, 3, 2]), 1);
assertEquals(part1([2, 1, 3]), 10);
assertEquals(part1([1, 2, 3]), 27);
assertEquals(part1([2, 3, 1]), 78);
assertEquals(part1([3, 2, 1]), 438);
assertEquals(part1([3, 1, 2]), 1836);

console.log("Result part 1: " + part1(input));

const part2 = (startingNumbers: number[]): number =>
  lastSpokenAfter(startingNumbers, 30000000);

console.log("Result part 2: " + part2(input));
