#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import { sum } from "../utils.ts";

const parseInput = (string: string): number[] =>
  string.trim().split(/[^0-9]+/)
    .map((entry) => Number(entry));

const example = parseInput(`
  35
  20
  15
  25
  47
  40
  62
  55
  65
  95
  102
  117
  150
  182
  127
  219
  299
  277
  309
  576
`);

assertEquals(example[0], 35);
assertEquals(example[1], 20);
assertEquals(example[19], 576);
assertEquals(example.length, 20);

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

const isSumOfTwo = (numbers: number[], target: number) => {
  for (let i = 0; i < numbers.length; i++) {
    for (let k = 0; k < numbers.length; k++) {
      if (i !== k && numbers[i] + numbers[k] === target) {
        return true;
      }
    }
  }

  return false;
};

const invalidIndices = (
  numbers: number[],
  preambleLength: number,
): number[] =>
  numbers.flatMap((n, i) =>
    i < preambleLength
      ? []
      : isSumOfTwo(numbers.slice(i - preambleLength, i), n)
      ? []
      : [i]
  );

const part1 = (numbers: number[], preambleLength = 25): number =>
  numbers[invalidIndices(numbers, preambleLength)[0]];

assertEquals(part1(example, 5), 127);

console.log("Result part 1: " + part1(inputParsed));

const part2 = (numbers: number[], preambleLength = 25): number => {
  const target = part1(numbers, preambleLength);

  for (let i = 0; i < numbers.length; i++) {
    for (let k = i + 1; k < numbers.length; k++) {
      const contiguousSet = numbers.slice(i, k + 1);
      if (sum(contiguousSet) === target) {
        contiguousSet.sort((a, b) => a - b);
        return contiguousSet[0] + contiguousSet[contiguousSet.length - 1];
      }
    }
  }
  throw new Error("No solution found");
};

assertEquals(part2(example, 5), 62);

console.log("Result part 2: " + part2(inputParsed));
