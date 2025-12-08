#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import { sum } from "../utils.ts";

type Bag = {
  color: string;
  contents: { color: string; quantity: number }[];
};

const parseInput = (string: string): Bag[] =>
  [...string.matchAll(
    /\W*(?<color>.*?) bags contain (?<contentsMatch>[^.]+)\./g,
  )]
    .map(({ groups }) => {
      if (!groups) throw new Error("?");
      const { color, contentsMatch } = groups;

      const contents = [
        ...contentsMatch.matchAll(/(?<quantityMatch>\d+) (?<color>.*?) bag/g),
      ].map(({ groups }) => {
        if (!groups) throw new Error("?");
        const { quantityMatch, color } = groups;

        return {
          color,
          quantity: Number(quantityMatch),
        };
      });

      return {
        color,
        contents,
      };
    });

const example = parseInput(`
  light red bags contain 1 bright white bag, 2 muted yellow bags.
  dark orange bags contain 3 bright white bags, 4 muted yellow bags.
  bright white bags contain 1 shiny gold bag.
  muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
  shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
  dark olive bags contain 3 faded blue bags, 4 dotted black bags.
  vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
  faded blue bags contain no other bags.
  dotted black bags contain no other bags.
`);

assertEquals(
  example[0],
  {
    color: "light red",
    contents: [
      { quantity: 1, color: "bright white" },
      { quantity: 2, color: "muted yellow" },
    ],
  },
);

const BEAUTIFUL_BAG = "shiny gold";

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

const directPossibilities = (bags: Bag[], color: string): Bag[] =>
  bags.filter((bag) => bag.contents.some((c) => c.color === color));

const recursivePossibilities = (bags: Bag[], color: string): Bag[] => {
  const direct = directPossibilities(bags, color);
  const forContents = direct.flatMap(({ color }) =>
    recursivePossibilities(bags, color)
  );

  return [...direct, ...forContents];
};

const part1 = (bags: Bag[]): number =>
  new Set(
    [...recursivePossibilities(bags, BEAUTIFUL_BAG).map((b) => b.color)],
  ).size;

assertEquals(part1(example), 4);

console.log("Result part 1: " + part1(inputParsed));

const recurseContents = (bags: Bag[], color: string): number => {
  const bag = bags.find((bag) => bag.color === color);
  if (!bag) throw new Error(`Referenced unknonw color ${color}`);

  return sum(
    bag.contents.map(({ color, quantity }) =>
      quantity * (1 + recurseContents(bags, color))
    ),
  );
};

const part2 = (bags: Bag[]): number => recurseContents(bags, BEAUTIFUL_BAG);

assertEquals(part2(example), 32);

const example2 = parseInput(`
  shiny gold bags contain 2 dark red bags.
  dark red bags contain 2 dark orange bags.
  dark orange bags contain 2 dark yellow bags.
  dark yellow bags contain 2 dark green bags.
  dark green bags contain 2 dark blue bags.
  dark blue bags contain 2 dark violet bags.
  dark violet bags contain no other bags.
`);

assertEquals(part2(example2), 126);

console.log("Result part 2: " + part2(inputParsed));
