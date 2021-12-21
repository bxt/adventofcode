#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { addCoords, Coord, matchGroups, range, sum } from "../../2020/utils.ts";

const matchInput = matchGroups(
  /Player \d+ starting position: (?<pos>\d+)/,
);

const parseInput = (string: string): number[] => {
  const lines = string.trim().split("\n");
  return lines.map((line) => parseInt(matchInput(line).pos, 10));
};

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

const part1 = (input: number[]): number => {
  console.log({ input });
  const positions = [...input];
  const scores = input.map(() => 0);
  let dice = 1;
  const roll = () => (dice++ - 1) % 100 + 1;

  while (true) {
    for (let i = 0; i < positions.length; i++) {
      const forward = roll() + roll() + roll();
      positions[i] = (positions[i] + forward - 1) % 10 + 1;
      scores[i] += positions[i];
      if (scores[i] >= 1000) {
        const rolls = dice - 1;
        const looserScore = scores[(i + 1) % scores.length];
        return rolls * looserScore;
      }
    }
  }
};

const example = parseInput(`
  Player 1 starting position: 4
  Player 2 starting position: 8
`);

assertEquals(part1(example), 739785);

console.log("Result part 1: " + part1(input));
