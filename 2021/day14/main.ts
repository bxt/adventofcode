#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { minBy } from "https://deno.land/std@0.116.0/collections/mod.ts";
import {
  Coord,
  CoordSet,
  ensureElementOf,
  minMax,
  range,
} from "../../2020/utils.ts";

type Input = {
  template: string;
  rules: Record<string, string>;
};

const parseInput = (string: string): Input => {
  const [templateString, rulesString] = string.trim().split("\n\n");

  const template = templateString.trim();

  const rules = Object.fromEntries(
    rulesString.trim().split("\n").map((line) => {
      const matches = line.match(/([A-Z][A-Z]) -> ([A-Z])/);
      if (!matches) throw new Error(`Does not match: "${line}"`);
      const [, from, insertion] = matches;
      return [from, insertion];
    }),
  );

  return { template, rules };
};

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

const part1 = (input: Input): number => {
  let result = input.template;

  for (let i = 0; i < 10; i++) {
    let newResult = "";
    for (let j = 0; j < result.length; j++) {
      const left = result.charAt(j);
      newResult += left;
      if (j < result.length - 1) {
        const right = result.charAt(j + 1);
        const insertion = input.rules[`${left}${right}`];
        if (insertion) {
          newResult += insertion;
        }
      }
    }
    result = newResult;
  }

  const counts: Record<string, number> = {};

  for (let j = 0; j < result.length; j++) {
    const left = result.charAt(j);
    counts[left] ??= 0;
    counts[left]++;
  }

  console.log({ counts });

  const [min, max] = minMax(Object.entries(counts).map(([, count]) => count));

  return max - min;
};

const example = parseInput(`
  NNCB

  CH -> B
  HH -> N
  CB -> H
  NH -> C
  HB -> C
  HC -> B
  HN -> C
  NN -> C
  BH -> H
  NC -> B
  NB -> B
  BN -> B
  BB -> N
  BC -> B
  CC -> N
  CN -> C
`);

assertEquals(part1(example), 1588);

console.log("Result part 1: " + part1(input));

const mergeCounts = (
  counts1: Record<string, number>,
  counts2: Record<string, number>,
) => {
  const counts: Record<string, number> = {};
  Object.entries(counts1).forEach(([key, count]) => {
    counts[key] = count;
  });
  Object.entries(counts2).forEach(([key, count]) => {
    counts[key] ??= 0;
    counts[key] += count;
  });
  return counts;
};

type Counts = Record<string, number>;

const addCounts = (
  into: Counts,
  counts: Counts,
) => {
  Object.entries(counts).forEach(([key, count]) => {
    into[key] ??= 0;
    into[key] += count;
  });
};

const part2 = (input: Input): number => {
  const elementCounts: Record<number, Record<string, Counts>> = {};
  const afterStep = 40;

  const result = input.template;
  const counts: Counts = {};

  const getCounts = (pair: string, iterationsLeft: number): Counts => {
    console.log({ pair, iterationsLeft });
    const counts: Counts = {};
    if (elementCounts[iterationsLeft]?.[pair]) {
      return elementCounts[iterationsLeft][pair];
    }
    const [left, right] = pair.split("");
    const insertion = input.rules[`${left}${right}`];
    if (insertion) {
      addCounts(counts, { [insertion]: 1 });
      if (iterationsLeft > 0) {
        addCounts(counts, getCounts(`${left}${insertion}`, iterationsLeft - 1));
        addCounts(
          counts,
          getCounts(`${insertion}${right}`, iterationsLeft - 1),
        );
      }
    }
    elementCounts[iterationsLeft] ??= {};
    elementCounts[iterationsLeft][pair] = counts;
    return counts;
  };

  for (let j = 0; j < result.length; j++) {
    const left = result.charAt(j);
    addCounts(counts, { [left]: 1 });
    if (j < result.length - 1) {
      const right = result.charAt(j + 1);
      addCounts(counts, getCounts(`${left}${right}`, afterStep - 1));
    }
  }

  console.log({ counts });

  const [min, max] = minMax(Object.entries(counts).map(([, count]) => count));

  console.log({ min, max });

  return max - min;
};

assertEquals(part2(example), 2188189693529);

console.log("Result part 2: " + part2(input));
