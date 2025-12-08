#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import {
  groupBy,
  maxWith,
  minWith,
} from "https://deno.land/std@0.116.0/collections/mod.ts";

const parseInput = (
  string: string,
): string[] => string.trim().split(/\n\W*/).map((line) => line.trim());

const text = await Deno.readTextFile("input.txt");

const numbers = parseInput(text);

type Comparator<T> = (t1: T, t2: T) => number;
type Aggregator<T> = (ts: T[], comparator: Comparator<T>) => T | undefined;

type Group = [string, string[]];

const compareGroups: Comparator<Group> = (
  [letter1, elements1],
  [letter2, elements2],
) => {
  const lengthsCompared = elements1.length - elements2.length;
  if (lengthsCompared !== 0) return lengthsCompared;
  return letter1 > letter2 ? 1 : -1;
};

assertEquals(compareGroups(["0", []], ["1", []]), -1);
assertEquals(compareGroups(["1", []], ["0", []]), 1);
assertEquals(compareGroups(["0", [""]], ["1", []]), 1);
assertEquals(compareGroups(["0", []], ["1", [""]]), -1);
assertEquals(compareGroups(["1", [""]], ["0", []]), 1);
assertEquals(compareGroups(["1", []], ["0", [""]]), -1);

const getBestGroupBy = (
  elements: string[],
  position: number,
  aggregator: Aggregator<Group>,
): Group => {
  const groups: Group[] = Object.entries(
    groupBy(elements, (element) => element.charAt(position)),
  );

  const bestGroup = aggregator(groups, compareGroups);

  if (!bestGroup) throw new Error("There were no elements to aggregate");

  return bestGroup;
};

const findBestBitsBy = (
  numbers: string[],
  aggregator: Aggregator<Group>,
): string => {
  const bestBits = numbers[0].split("").map((_, i) => {
    const group = getBestGroupBy(numbers, i, aggregator);
    return group[0];
  });

  return bestBits.join("");
};

const part1 = (numbers: string[]): number => {
  const mins = findBestBitsBy(numbers, minWith);
  const maxs = findBestBitsBy(numbers, maxWith);

  const epsilonRate = parseInt(mins, 2);
  const gammaRate = parseInt(maxs, 2);

  return epsilonRate * gammaRate;
};

const example = parseInput(`
  00100
  11110
  10110
  10111
  10101
  01111
  00111
  11100
  10000
  11001
  00010
  01010
`);

assertEquals(part1(example), 198, "Example is wrong!");

console.log("Result part 1: " + part1(numbers));

const fiilterDownNumbersBy = (
  numbers: string[],
  aggregator: Aggregator<Group>,
): string => {
  let filteredNumbers = [...numbers];

  for (let i = 0; i < filteredNumbers[0].length; i++) {
    const group = getBestGroupBy(filteredNumbers, i, aggregator);

    filteredNumbers = group[1];

    if (filteredNumbers.length === 1) return filteredNumbers[0];
  }

  throw new Error(
    `After filtering by all bits there are ${filteredNumbers.length} numbers left instead of 1.`,
  );
};

const part2 = (numbers: string[]): number => {
  const oxy = fiilterDownNumbersBy(numbers, maxWith);
  const co2 = fiilterDownNumbersBy(numbers, minWith);

  const oxyNumber = parseInt(oxy, 2);
  const co2Number = parseInt(co2, 2);

  return oxyNumber * co2Number;
};

assertEquals(part2(example), 230, "Example is wrong!");

console.log("Result part 2: " + part2(numbers));
