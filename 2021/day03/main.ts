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
