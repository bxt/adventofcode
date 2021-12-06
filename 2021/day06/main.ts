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

const singleBreedCount = (number: number, iterations: number) => {
  return Math.floor((6 - number + iterations) / 7);
};

assertEquals(singleBreedCount(0, 18), 3);
assertEquals(singleBreedCount(1, 18), 3);
assertEquals(singleBreedCount(2, 18), 3);
assertEquals(singleBreedCount(3, 18), 3);
assertEquals(singleBreedCount(4, 18), 2);
assertEquals(singleBreedCount(5, 18), 2);
assertEquals(singleBreedCount(6, 18), 2);
assertEquals(singleBreedCount(7, 18), 2);
assertEquals(singleBreedCount(8, 18), 2);
assertEquals(singleBreedCount(9, 18), 2);
assertEquals(singleBreedCount(10, 18), 2);
assertEquals(singleBreedCount(11, 18), 1);

assertEquals(singleBreedCount(4, 19), 3);
assertEquals(singleBreedCount(5, 19), 2);
assertEquals(singleBreedCount(6, 19), 2);

assertEquals(singleBreedCount(4, 20), 3);
assertEquals(singleBreedCount(5, 20), 3);
assertEquals(singleBreedCount(6, 20), 2);

assertEquals(singleBreedCount(4, 21), 3);
assertEquals(singleBreedCount(5, 21), 3);
assertEquals(singleBreedCount(6, 21), 3);

const unlimitedBreedCount = (initialState: number[], iterations: number) => {
  const totalSpawns: Record<number, number> = {};

  const getTotalSpawns = (number: number): number => {
    if (totalSpawns[number] !== undefined) return totalSpawns[number];

    const selfReplicatesTimes = singleBreedCount(number, iterations);
    // console.log({ number, selfReplicatesTimes });
    if (selfReplicatesTimes < 1) return 1;

    const childNumbers = [];
    for (let i = 0; i < selfReplicatesTimes; i++) {
      const childNumber = 7 * (i + 1) + number + 2;
      childNumbers.push(childNumber);
    }
    // console.log({ childNumbers });
    const result = 1 + sum(childNumbers.map(getTotalSpawns));
    totalSpawns[number] = result;
    return result;
  };

  const result = sum(initialState.map(getTotalSpawns));
  // console.log({ totalSpawns });
  return result;
};

const part2 = (input: number[]): number => {
  return unlimitedBreedCount(input, 256);
};

assertEquals(unlimitedBreedCount(example, 18), 26);
assertEquals(unlimitedBreedCount(example, 80), 5934);
assertEquals(unlimitedBreedCount(input, 80), 379414);

assertEquals(part2(example), 26984457539);

console.log("Result part 2: " + part2(input));
