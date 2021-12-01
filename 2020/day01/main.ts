#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";

const TARGET = 2020;

const parseInput = (string: string): number[] =>
  string.trim().split(/\W+/).map(Number);

const text = await Deno.readTextFile("input.txt");

const entries = parseInput(text);

const part1 = (numbers: number[]): number => {
  for (let i = 0; i < numbers.length; i++) {
    for (let k = i + 1; k < numbers.length; k++) {
      if (numbers[i] + numbers[k] === TARGET) {
        return numbers[i] * numbers[k];
      }
    }
  }

  throw new Error("Not found");
};

const example = parseInput(`
  1721
  979
  366
  299
  675
  1456
`);

assertEquals(part1(example), 514579, "Example is wrong!");

console.log("Result part 1: " + part1(entries));

const part2 = (numbers: number[]): number => {
  for (let i = 0; i < numbers.length; i++) {
    for (let k = i + 1; k < numbers.length; k++) {
      for (let l = k + 1; l < numbers.length; l++) {
        if (numbers[i] + numbers[k] + numbers[l] === TARGET) {
          return numbers[i] * numbers[k] * numbers[l];
        }
      }
    }
  }

  throw new Error("Not found");
};

assertEquals(part2(example), 241861950, "Example is wrong!");

console.log("Result part 2: " + part2(entries));
