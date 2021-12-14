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
