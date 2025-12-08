#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import { addCoords, Coord, ensureElementOf } from "../../2020/utils.ts";

const directions = ["forward", "down", "up"] as const;

type Direction = typeof directions[number];

type Step = { direction: Direction; amount: number };

const parseInput = (
  string: string,
): Step[] =>
  string.trim().split(/\n\W*/).map((line) => {
    const [directionString, amountString] = line.split(" ");
    return {
      direction: ensureElementOf(directionString, directions),
      amount: parseInt(amountString, 10),
    };
  });

const text = await Deno.readTextFile("input.txt");

const entries = parseInput(text);

const part1 = (steps: Step[]): number => {
  const [position, depth] = steps.map(({ direction, amount }): Coord => {
    switch (direction) {
      case "forward":
        return [amount, 0];
      case "down":
        return [0, amount];
      case "up":
        return [0, -amount];
    }
  }).reduce(addCoords);

  return position * depth;
};

const example = parseInput(`
  forward 5
  down 5
  forward 8
  up 3
  down 8
  forward 2
`);

assertEquals(part1(example), 150, "Example is wrong!");

console.log("Result part 1: " + part1(entries));

type State = { aim: number; position: number; depth: number };

const part2 = (steps: Step[]): number => {
  const initialState: State = { aim: 0, position: 0, depth: 0 };

  const result = steps.reduce(
    (prev: State, { direction, amount }): State => {
      const { aim, position, depth } = prev;
      switch (direction) {
        case "forward":
          return {
            ...prev,
            position: position + amount,
            depth: depth + aim * amount,
          };
        case "down":
          return { ...prev, aim: aim + amount };
        case "up":
          return { ...prev, aim: aim - amount };
      }
    },
    initialState,
  );

  const { position, depth } = result;
  return position * depth;
};

assertEquals(part2(example), 900, "Example is wrong!");

console.log("Result part 2: " + part2(entries));
