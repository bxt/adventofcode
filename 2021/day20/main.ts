#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { addCoords, Coord, range, sum } from "../../2020/utils.ts";

type Input = { rule: string; image: string[][]; infinity: string };

const parseInput = (string: string): Input => {
  const [ruleString, imageString] = string.trim().split("\n\n");
  const rule = ruleString.replaceAll(/\s/g, "");
  const image = imageString.split("\n").map((line) => line.trim().split(""));
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
    infinity: rule.charAt(infinity === "." ? 0 : 0b111_111_111),
    image: range(image.length + 2).map((lineNumber) =>
      range(image[0].length + 2).map((colNumber) => {
        const centerCoord: Coord = [colNumber - 1, lineNumber - 1];
        const binaryCoords = binaryDigitDirections.map((c) =>
          addCoords(c, centerCoord)
        );
        const binaryShapes = binaryCoords.map((c) =>
          image?.[c[1]]?.[c[0]] ?? infinity
        ).join("");
        const binaryNumber = binaryShapes
          .replaceAll(".", "0")
          .replaceAll("#", "1");
        const number = parseInt(binaryNumber, 2);
        const replacement = rule.charAt(number);
        return replacement;
      })
    ),
  };
}

const counterActiveAfter = (input: Input, interations: number): number => {
  let output = input;
  range(interations).forEach(() => {
    output = enhance(output);
  });
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
