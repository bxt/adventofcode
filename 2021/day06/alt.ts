#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import { sum } from "../../2020/utils.ts";
import {
  BREED_DAYS,
  DAYS_PART_ONE,
  DAYS_PART_TWO,
  example,
  input,
  MATURE_DAYS,
} from "./main.ts";

const breed = (initialState: number[], iterations: number) => {
  const state = Array(MATURE_DAYS + 1).fill(0);
  initialState.forEach((n) => state[n]++);

  for (let i = 0; i < iterations; i++) {
    const breedCount = state.shift();
    state[BREED_DAYS - 1] += breedCount;
    state.push(breedCount);
  }

  return sum(state);
};

const part1 = (input: number[]): number => {
  return breed(input, DAYS_PART_ONE);
};

assertEquals(part1(example), 5934);

console.log("Result part 1: " + part1(input));

const part2 = (input: number[]): number => {
  return breed(input, DAYS_PART_TWO);
};

assertEquals(part2(example), 26984457539);

console.log("Result part 2: " + part2(input));
