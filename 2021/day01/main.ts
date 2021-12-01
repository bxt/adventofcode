#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { sum } from "../../2020/utils.ts";

const TARGET = 2020;

const parseInput = (string: string): number[] =>
  string.trim().split(/\W+/).map(Number);

const text = await Deno.readTextFile("input.txt");

const entries = parseInput(text);

const windowIncreases = (numbers: number[], windowSize: number): number => {
  let count = 0;
  for (let i = 0; i < numbers.length - windowSize; i++) {
    if (
      sum(numbers.slice(i, i + windowSize)) <
        sum(numbers.slice(i + 1, i + windowSize + 1))
    ) {
      count++;
    }
  }

  return count;
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
