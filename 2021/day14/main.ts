#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { minMax } from "../../2020/utils.ts";

type Input = {
  template: string;
  rules: Record<string, string>;
};

type Counts = Record<string, number>;

const parseInput = (string: string): Input => {
  const [templateString, rulesString] = string.trim().split("\n\n");

  const template = templateString.trim();

  const rules = Object.fromEntries(
    rulesString.trim().split("\n").map((line) => {
      const matches = line.match(/([A-Z]{2}) -> ([A-Z])/);
      if (!matches) throw new Error(`Does not match: "${line}"`);
      const [, from, insertion] = matches;
      return [from, insertion];
    }),
  );

  return { template, rules };
};

const addCounts = (into: Counts, counts: Counts) => {
  Object.entries(counts).forEach(([key, count]) => {
    into[key] ??= 0;
    into[key] += count;
  });
};

const getMinMaxCountDiffAfter = (
  { template, rules }: Input,
  afterStep: number,
): number => {
  const elementCounts: Record<number, Record<string, Counts>> = {};

  const getCounts = (
    left: string,
    right: string,
    iterationsLeft: number,
  ): Counts => {
    const pair = `${left}${right}`;

    if (elementCounts[iterationsLeft]?.[pair]) {
      return elementCounts[iterationsLeft][pair];
    }

    const counts: Counts = {};
    const insertion = rules[pair];

    if (insertion) {
      addCounts(counts, { [insertion]: 1 });

      if (iterationsLeft > 0) {
        addCounts(counts, getCounts(left, insertion, iterationsLeft - 1));
        addCounts(counts, getCounts(insertion, right, iterationsLeft - 1));
      }
    }

    elementCounts[iterationsLeft] ??= {};
    elementCounts[iterationsLeft][pair] = counts;
    return counts;
  };

  const counts: Counts = {};

  for (let position = 0; position < template.length; position++) {
    const left = template.charAt(position);
    addCounts(counts, { [left]: 1 });

    if (position < template.length - 1) {
      const right = template.charAt(position + 1);
      addCounts(counts, getCounts(left, right, afterStep - 1));
    }
  }

  const [min, max] = minMax(Object.entries(counts).map(([, count]) => count));

  return max - min;
};

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

const part1 = (input: Input): number => {
  return getMinMaxCountDiffAfter(input, 10);
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

const part2 = (input: Input): number => {
  return getMinMaxCountDiffAfter(input, 40);
};

assertEquals(part2(example), 2188189693529);

console.log("Result part 2: " + part2(input));
