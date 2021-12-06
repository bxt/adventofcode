#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { sum } from "../../2020/utils.ts";

const parseInput = (string: string): number[] => {
  return string.trim().split(",").map((s) => parseInt(s, 10));
};

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

const breed = (initialState: number[], iterations: number) => {
  let state = initialState;

  for (let i = 0; i < iterations; i++) {
    state = state.flatMap((s) => s <= 0 ? [6, 8] : [s - 1]);
  }
  return state;
};

const part1 = (input: number[]): number => {
  return breed(input, 80).length;
};

const example = parseInput(`3,4,3,1,2`);

assertEquals(breed(example, 18).length, 26);
assertEquals(part1(example), 5934);

console.log("Result part 1: " + part1(input));

// const part2 = (input: number[]): number => {
//   sum(input.map((number) => {
//     return 1;
//   }));
// };

// assertEquals(part2(example), 26984457539);

// console.log("Result part 2: " + part2(input));
