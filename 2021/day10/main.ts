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

const parseInput = (string: string): string[][] =>
  string.trim().split("\n").map((line) => {
    return line.trim().split("");
  });

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

const isClosingBrace = (character: string): character is ClosingBrace =>
  Object.keys(scores).includes(character);

const part1 = (input: string[][]): number => {
  return sum(input.map((line) => {
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
          return scores[character];
        }
      } else {
        stack.push(character);
      }
    }
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

// assertEquals(part2(example), 1134);

// console.log("Result part 2: " + part2(input));
