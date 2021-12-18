#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { mapValues } from "https://deno.land/std@0.116.0/collections/mod.ts";
import { matchGroups } from "../../2020/utils.ts";

type Input = { x1: number; x2: number; y1: number; y2: number };

const matchInput = matchGroups(
  /target area: x=(?<x1>-?\d+)..(?<x2>-?\d+), y=(?<y1>-?\d+)..-(?<y2>-?\d+)/,
);

const parseInput = (string: string): Input => {
  const { x1, x2, y1, y2 } = mapValues(
    matchInput(string),
    (s) => parseInt(s, 10),
  );
  return { x1, x2, y1, y2 };
};

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

function part1(input: Input): number {
  console.log({ input });
  return 1;
}

const example = parseInput(`
  target area: x=20..30, y=-10..-5
`);

assertEquals(part1(example), 45);

console.log("Result part 1: " + part1(input));
