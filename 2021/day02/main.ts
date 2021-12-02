#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { addCoords, Coord, ensureElementOf } from "../../2020/utils.ts";

const directions = ["forward", "down", "up"] as const;

type Direction = typeof directions[number];

const parseInput = (
  string: string,
): { direction: Direction; amount: number }[] =>
  string.trim().split(/\n\W*/).map((line) => {
    const [directionString, amountString] = line.split(" ");
    return {
      direction: ensureElementOf(directionString, directions),
      amount: parseInt(amountString, 10),
    };
  });

const text = await Deno.readTextFile("input.txt");

const entries = parseInput(text);

const part1 = (steps: { direction: Direction; amount: number }[]): number => {
  const [positon, depth] = steps.map(({ direction, amount }): Coord => {
    if (direction === "forward") return [amount, 0];
    if (direction === "down") return [0, amount];
    if (direction === "up") return [0, -amount];
    return [0, 0];
  }).reduce(addCoords);
  return positon * depth;
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

const part2 = (steps: { direction: Direction; amount: number }[]): number => {
  const initialState: State = { aim: 0, position: 0, depth: 0 };
  const result = steps.reduce(
    (prev: State, { direction, amount }) => {
      const { aim, position, depth } = prev;
      if (direction === "forward") {
        return {
          ...prev,
          position: position + amount,
          depth: depth + aim * amount,
        };
      }
      if (direction === "down") return { ...prev, aim: aim + amount };
      if (direction === "up") return { ...prev, aim: aim - amount };
      throw new Error();
    },
    initialState,
  );

  const { position, depth } = result;
  return position * depth;
};

assertEquals(part2(example), 900, "Example is wrong!");

console.log("Result part 2: " + part2(entries));
