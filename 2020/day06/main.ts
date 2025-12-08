#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import { intersectSets, sum } from "../utils.ts";

const parseInput = (string: string): string[][][] =>
  string.trim().split("\n\n").map((block) => (
    block.trim().split(/[ \n]+/).map((line) => line.split(""))
  ));

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

const example = parseInput(`
  abc

  a
  b
  c

  ab
  ac

  a
  a
  a
  a

  b
`);

const sumCounts = (groups: string[][][]): number =>
  sum(groups.map((group) => new Set(group.flat()).size));

assertEquals(sumCounts(example), 11);

const part1 = sumCounts(inputParsed);

console.log("Result part 1: " + part1);

const sumIntersectionCounts = (groups: string[][][]): number =>
  sum(
    groups.map((group) =>
      intersectSets(...group.map((person) => new Set(person))).size
    ),
  );

assertEquals(
  sumIntersectionCounts(example),
  6,
);

const part2 = sumIntersectionCounts(inputParsed);

console.log("Result part 2: " + part2);
