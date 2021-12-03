#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import {
  aggregateGroups,
  groupBy,
  maxBy,
  minBy,
} from "https://deno.land/std@0.116.0/collections/mod.ts";

const parseInput = (
  string: string,
): string[] => string.trim().split(/\n\W*/).map((line) => line.trim());

const text = await Deno.readTextFile("input.txt");

const numbers = parseInput(text);

const part1 = (numbers: string[]): number => {
  const mostCommonBits = numbers[0].split("").map((_, i) => {
    const groups = groupBy(numbers.map((line) => line.charAt(i)), (c) => c);
    const bitCountsAtI = Object.entries(aggregateGroups(
      groups,
      (current, _, first, acc) =>
        first ? current.length : acc as number + current.length,
    ));

    const [max] = maxBy(
      bitCountsAtI,
      ([_, count]) => count as number,
    ) as unknown as [
      number,
    ];
    const [min] = minBy(
      bitCountsAtI,
      ([_, count]) => count as number,
    ) as unknown as [
      number,
    ];

    return [min, max];
  });

  const mins = mostCommonBits.map(([m, _]) => m).join("");
  const maxs = mostCommonBits.map(([_, m]) => m).join("");

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

const part2helper = (numbers: string[], agg: any, tieVal: string): string => {
  let filteredNumbers = [...numbers];

  for (let i = 0; i < filteredNumbers[0].length; i++) {
    const groups = groupBy(
      filteredNumbers.map((line) => line.charAt(i)),
      (c) => c,
    );
    const bitCountsAtI = Object.entries(aggregateGroups(
      groups,
      (current, _, first, acc) =>
        first ? current.length : acc as number + current.length,
    ));

    const set = new Set(bitCountsAtI.map(([_, c]) => c));

    const selected = set.size === 1 ? tieVal : (agg(
      bitCountsAtI,
      ([_, count]: [any, any]) => count as number,
    ) as unknown as [
      string,
    ])[0];

    filteredNumbers = filteredNumbers.filter((number) =>
      number.charAt(i) === selected
    );

    if (filteredNumbers.length === 1) return filteredNumbers[0];
  }

  throw new Error();
};

const part2 = (numbers: string[]): number => {
  const oxy = part2helper(numbers, maxBy, "1");
  const co2 = part2helper(numbers, minBy, "0");

  const oxyDec = parseInt(oxy, 2);
  const co2Dec = parseInt(co2, 2);

  return oxyDec * co2Dec;
};

assertEquals(part2(example), 230, "Example is wrong!");

console.log("Result part 2: " + part2(numbers));
