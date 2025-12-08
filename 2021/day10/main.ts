#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import { sum } from "../../2020/utils.ts";

const scores = { ")": 3, "]": 57, "}": 1197, ">": 25137 };
type ClosingBrace = keyof typeof scores;

const completionScores = { "(": 1, "[": 2, "{": 3, "<": 4 };
type OpeningBrace = keyof typeof completionScores;

const matchingBraces: Record<ClosingBrace, OpeningBrace> = {
  ")": "(",
  "]": "[",
  "}": "{",
  ">": "<",
};

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

type UnexpectedResult = { unexpected: ClosingBrace };
type UnmatchedResult = { unmatched: OpeningBrace[] };
type OkayResult = { okay: true };
type Result = UnexpectedResult | UnmatchedResult | OkayResult;

const isUnexpectedResult = (result: Result): result is UnexpectedResult =>
  "unexpected" in result;
const isUnmatchedResult = (result: Result): result is UnmatchedResult =>
  "unmatched" in result;

const checkLine = (line: string[]): Result => {
  const stack: OpeningBrace[] = [];
  for (const character of line) {
    if (isClosingBrace(character)) {
      if (matchingBraces[character] !== stack.pop()) {
        return { unexpected: character };
      }
    } else if (isOpeningBrace(character)) {
      stack.push(character);
    } else {
      // ignore everything other than braces
    }
  }
  if (stack.length > 0) {
    return { unmatched: stack };
  } else {
    return { okay: true };
  }
};

const part1 = (input: string[][]): number => {
  return sum(
    input.map(checkLine).filter(isUnexpectedResult).map((result) =>
      scores[result.unexpected]
    ),
  );
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

const median = (values: number[]) =>
  [...values].sort((a, b) => b - a)[Math.floor((values.length - 1) / 2)];

assertEquals(median([4, 5, 6]), 5);
assertEquals(median([6, 4, 5]), 5);
assertEquals(median([288957, 5566, 1480781, 995444, 294]), 288957);

const part2 = (input: string[][]): number => {
  return median(
    input.map(checkLine).filter(isUnmatchedResult).map((result) =>
      result.unmatched.reduceRight(
        (acc, character) => acc * 5 + completionScores[character],
        0,
      )
    ),
  );
};

assertEquals(part2(example), 288957);

console.log("Result part 2: " + part2(input));
