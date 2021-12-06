#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { sum } from "../../2020/utils.ts";

const BREED_DAYS = 7;
const MATURE_DAYS = 8;

const parseInput = (string: string): number[] => {
  return string.trim().split(",").map((s) => parseInt(s, 10));
};

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

const childrenDaysToBreed = (
  daysToBreed: number,
  daysToGo: number,
): number[] => {
  const childNumbers = [];

  for (let day = 1; day <= daysToGo; day++) {
    if (daysToBreed === 0) {
      daysToBreed = BREED_DAYS;
      childNumbers.push(day + MATURE_DAYS);
    }
    daysToBreed--;
  }

  return childNumbers;
};

assertEquals(childrenDaysToBreed(3, 18), [12, 19, 26]);
assertEquals(childrenDaysToBreed(12, 18), [21]);
assertEquals(childrenDaysToBreed(1, 18), [10, 17, 24]);
assertEquals(childrenDaysToBreed(10, 18), [19, 26]);
assertEquals(childrenDaysToBreed(17, 18), [26]);
[19, 21, 24, 26].forEach((n) => {
  assertEquals(childrenDaysToBreed(n, 18), []);
});

const memoize = <R>(func: (n: number) => R): (n: number) => R => {
  const memory: Record<number, R> = {};

  const memoizedFunc = (n: number): R => {
    memory[n] ??= func(n);
    return memory[n];
  };

  return memoizedFunc;
};

const unlimitedBreedCount = (
  daysToBreeds: number[],
  daysToGo: number,
): number => {
  const getTotalSpawns = memoize((daysToBreed: number): number =>
    1 + sum(
      childrenDaysToBreed(daysToBreed, daysToGo).map(getTotalSpawns),
    )
  );

  return sum(daysToBreeds.map(getTotalSpawns));
};

const part1 = (input: number[]): number => {
  return unlimitedBreedCount(input, 80);
};

const example = parseInput(`3,4,3,1,2`);

assertEquals(unlimitedBreedCount(example, 18), 26);
assertEquals(part1(example), 5934);

console.log("Result part 1: " + part1(input));

const part2 = (input: number[]): number => {
  return unlimitedBreedCount(input, 256);
};

assertEquals(part2(example), 26984457539);

console.log("Result part 2: " + part2(input));
