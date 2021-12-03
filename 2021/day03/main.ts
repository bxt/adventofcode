#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import {
  aggregateGroups,
  groupBy,
  maxBy,
  minBy,
} from "https://deno.land/std@0.116.0/collections/mod.ts";
import { addCoords, Coord } from "../../2020/utils.ts";

const text = await Deno.readTextFile("input.txt");

const part1 = (input: string): number => {
  const lines = input.trim().split("\n");

  console.log({ lines, ll: lines[0].split("") });

  const mostCommonBits = lines[0].split("").map((_, i) => {
    const groups = groupBy(lines.map((line) => line.charAt(i)), (c) => c);
    console.log({ groups });
    const bitCountsAtI = Object.entries(aggregateGroups(
      groups,
      (current, _, first, acc) =>
        first ? current.length : acc as number + current.length,
    ));

    console.log({ i, bitCountsAtI });

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

  console.log({ mostCommonBits });

  const mins = mostCommonBits.map(([m, _]) => m).join("");
  const maxs = mostCommonBits.map(([_, m]) => m).join("");
  console.log({ mins, maxs });

  const epsilonRate = parseInt(mins, 2);
  const gammaRate = parseInt(maxs, 2);

  console.log({ epsilonRate, gammaRate });

  return epsilonRate * gammaRate;
};

const example = `
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
`;

assertEquals(part1(example), 198, "Example is wrong!");

console.log("Result part 1: " + part1(text));

const part2helper = (input: string, agg: any, tieVal: string): string => {
  const lines = input.trim().split("\n");

  let numbers = lines;

  for (let i = 0; i < lines[0].length; i++) {
    const groups = groupBy(numbers.map((line) => line.charAt(i)), (c) => c);
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

    numbers = numbers.filter((number) => number.charAt(i) === selected);

    console.log({ i, bitCountsAtI, selected, numbers, set });

    if (numbers.length === 1) return numbers[0];
  }

  throw new Error();
};

const part2 = (input: string): number => {
  const oxy = part2helper(input, maxBy, "1");
  const co2 = part2helper(input, minBy, "0");

  console.log({ oxy, co2 });

  const oxyDec = parseInt(oxy, 2);
  const co2Dec = parseInt(co2, 2);

  console.log({ oxyDec, co2Dec });

  return oxyDec * co2Dec;
};

assertEquals(part2(example), 230, "Example is wrong!");

console.log("Result part 2: " + part2(text));
