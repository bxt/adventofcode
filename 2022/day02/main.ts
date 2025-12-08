#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import { ensureElementOf, sum } from "../../2020/utils.ts";

type RPS = "rock" | "paper" | "scissors";
type Round = { opponent: RPS; you: RPS };

const parseInput = (string: string): Round[] =>
  string.trim().split("\n").map((line) => {
    const [opponentRaw, youRaw] = line.trim().split(" ");
    const tokens = ["A", "B", "C", "X", "Y", "Z"] as const;
    type Tokens = typeof tokens[number];
    const rpsMap: Record<Tokens, RPS> = {
      A: "rock",
      B: "paper",
      C: "scissors",
      X: "rock",
      Y: "paper",
      Z: "scissors",
    };
    const opponentChecked = ensureElementOf(opponentRaw, tokens);
    const youChecked = ensureElementOf(youRaw, tokens);
    return { opponent: rpsMap[opponentChecked], you: rpsMap[youChecked] };
  });

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

const part1 = (input: Round[]): number => {
  const roundScores = input.map((round) => {
    const { opponent, you } = round;
    const key: `${RPS} ${RPS}` = `${opponent} ${you}`;
    const scoreMap = {
      "rock rock": 3 + 1,
      "rock paper": 6 + 2,
      "rock scissors": 0 + 3,
      "paper rock": 0 + 1,
      "paper paper": 3 + 2,
      "paper scissors": 6 + 3,
      "scissors rock": 6 + 1,
      "scissors paper": 0 + 2,
      "scissors scissors": 3 + 3,
    };
    return scoreMap[key];
  });
  return sum(roundScores);
};

const part2 = (input: Round[]): number => {
  const roundScores = input.map((round) => {
    const { opponent, you } = round;
    const key: `${RPS} ${RPS}` = `${opponent} ${you}`;
    const scoreMap = {
      "rock rock": 0 + 3,
      "rock paper": 3 + 1,
      "rock scissors": 6 + 2,
      "paper rock": 0 + 1,
      "paper paper": 3 + 2,
      "paper scissors": 6 + 3,
      "scissors rock": 0 + 2,
      "scissors paper": 3 + 3,
      "scissors scissors": 6 + 1,
    };
    return scoreMap[key];
  });
  return sum(roundScores);
};

const example = parseInput(`
  A Y
  B X
  C Z
`);

assertEquals(part1(example), 15);

console.log("Result part 1: " + part1(input));

assertEquals(part2(example), 12);

console.log("Result part 2: " + part2(input));
