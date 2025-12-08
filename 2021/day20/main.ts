#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import {
  addCoords,
  Coord,
  ensureElementOf,
  range,
  sum,
} from "../../2020/utils.ts";

const letters = [".", "#"] as const;
type Letter = typeof letters[number];

type Input = { rule: Letter[]; image: Letter[][]; infinity: Letter };

const parseLine = (line: string): Letter[] => {
  return line.trim().split("").map((l) => ensureElementOf(l, letters));
};

const parseInput = (string: string): Input => {
  const [ruleString, imageString] = string.trim().split("\n\n");
  const rule = parseLine(ruleString.replaceAll(/\s/g, ""));
  const image = imageString.split("\n").map(parseLine);
  return { rule, image, infinity: "." };
};

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

function countActive(input: Input) {
  if (input.infinity === "#") return Infinity;
  return sum(
    input.image.map((line) => line.filter((letter) => letter === "#").length),
  );
}

const binaryDigitDirections: Coord[] = [
  [-1, -1],
  [+0, -1],
  [+1, -1],
  [-1, +0],
  [+0, +0],
  [+1, +0],
  [-1, +1],
  [+0, +1],
  [+1, +1],
];

function enhance({ rule, image, infinity }: Input) {
  return {
    rule,
    infinity: rule[infinity === "." ? 0 : 0b111_111_111],
    image: range(image.length + 2).map((lineNumber) =>
      range(image[0].length + 2).map((colNumber) => {
        const centerCoord: Coord = [colNumber - 1, lineNumber - 1];
        const binaryCoords = binaryDigitDirections.map((c) =>
          addCoords(c, centerCoord)
        );
        const binaryShapes = binaryCoords.map((c) =>
          image?.[c[1]]?.[c[0]] ?? infinity
        );
        const binaryNumber = binaryShapes.join("")
          .replaceAll(".", "0")
          .replaceAll("#", "1");
        const number = parseInt(binaryNumber, 2);
        const replacement = rule[number];
        return replacement;
      })
    ),
  };
}

const counterActiveAfter = (input: Input, interations: number): number => {
  const output = range(interations).reduce(enhance, input);
  return countActive(output);
};

const part1 = (input: Input): number => {
  return counterActiveAfter(input, 2);
};

const example = parseInput(`
  ..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
  #..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
  .######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
  .#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
  .#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
  ...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
  ..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

  #..#.
  #....
  ##..#
  ..#..
  ..###
`);

assertEquals(part1(example), 35);

console.log("Result part 1: " + part1(input));

const part2 = (input: Input): number => {
  return counterActiveAfter(input, 50);
};

assertEquals(part2(example), 3351);

console.log("Result part 2: " + part2(input));
