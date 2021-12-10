#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import {
  Coord,
  CoordSet,
  indexWithCoord,
  product,
  sum,
} from "../../2020/utils.ts";

const scores = { ")": 3, "]": 57, "}": 1197, ">": 25137 };
const matchingBraces = { ")": "(", "]": "[", "}": "{", ">": "<" };
type ClosingBrace = keyof typeof scores;

const completionScores = { "(": 1, "[": 2, "{": 3, "<": 4 };
type OpeningBrace = keyof typeof completionScores;

const parseInput = (string: string): string[][] =>
  string.trim().split("\n").map((line) => {
    return line.trim().split("");
  });

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

const isClosingBrace = (character: string): character is ClosingBrace =>
  Object.keys(scores).includes(character);

const isOpeningBrace = (character: string): character is OpeningBrace =>
  Object.keys(completionScores).includes(character);

const checkLine = (line: string[]) => {
  const stack = [];
  for (const character of line) {
    if (isClosingBrace(character)) {
      const tip = stack.pop();
      if (matchingBraces[character] !== tip) {
        console.log({
          expected: matchingBraces[character],
          found: tip,
          line,
        });
        return { unexpected: character };
      }
    } else {
      stack.push(character);
    }
  }
  if (stack.length > 0) {
    return { unmatched: stack };
  } else {
    return { okay: true };
  }
};

const part1 = (input: string[][]): number => {
  return sum(input.map((line) => {
    const result = checkLine(line);
    if (result.unexpected) return scores[result.unexpected];
    return 0;
  }));
};

const example = parseInput(`
  [({(<(())[]>[[{[]{<()<>>
  [(()[<>])]({[<{<<[]>>(
  {([(<{}[<>[]}>{[]{[(<()>
  (((({<>}<{<{<>}{[]{[]{}
  [[<[([]))<([[{}[[()]]]
  [{[{({}]{}}([{[{{{}}([]
  {<[[]]>}<{[{[{[]{()[[[]
  [<(<(<(<{}))><([]([]()
  <{([([[(<>()){}]>(<<{{
  <{([{{}}[<[[[<>{}]]]>[]]
`);

assertEquals(part1(example), 26397);

console.log("Result part 1: " + part1(input));

export const largestBasinRating = (basins: CoordSet[]) => {
  const basinSizes = basins.map((b) => b.size).sort((a, b) => b - a);
  const largestSizes = basinSizes.slice(0, 3);
  return product(largestSizes);
};

const median = (values: number[]) =>
  [...values].sort((a, b) => b - a)[Math.floor((values.length - 1) / 2)];

assertEquals(median([4, 5, 6]), 5);
assertEquals(median([6, 4, 5]), 5);
assertEquals(median([288957, 5566, 1480781, 995444, 294]), 288957);

const part2 = (input: string[][]): number => {
  const socres = input.map((line) => {
    const result = checkLine(line);

    if (!result.unmatched) return undefined;

    return result.unmatched.reduceRight((acc, character) => {
      if (!isOpeningBrace(character)) throw new Error(character);
      const x = completionScores[character];
      console.log({ character, x, acc });
      return acc * 5 + x;
    }, 0);
  }).filter((x): x is number => x !== undefined);
  console.log({ socres });

  return median(socres);
};

assertEquals(part2(example), 288957);

console.log("Result part 2: " + part2(input));
