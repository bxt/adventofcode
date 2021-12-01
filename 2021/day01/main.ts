#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { slidingWindows } from "https://deno.land/std@0.116.0/collections/mod.ts";
import { sum } from "../../2020/utils.ts";

const parseInput = (string: string): number[] =>
  string.trim().split(/\W+/).map(Number);

const text = await Deno.readTextFile("input.txt");

const entries = parseInput(text);

const windowIncreases = (numbers: number[], windowSize: number): number => {
  const windows = slidingWindows(numbers, windowSize).map(sum);
  return slidingWindows(windows, 2).filter(([a, b]) => a < b).length;
};

const part1 = (numbers: number[]): number => windowIncreases(numbers, 1);

const example = parseInput(`
  199
  200
  208
  210
  200
  207
  240
  269
  260
  263
`);

assertEquals(part1(example), 7, "Example is wrong!");

console.log("Result part 1: " + part1(entries));

const part2 = (numbers: number[]): number => windowIncreases(numbers, 3);

assertEquals(part2(example), 5, "Example is wrong!");

console.log("Result part 2: " + part2(entries));
