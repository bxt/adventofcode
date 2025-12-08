#!/usr/bin/env deno run --allow-read=input.txt

const WORD = "XMAS";

const file = await Deno.readTextFile("input.txt");

const lines = file.split("\n").filter((line) => line);

const fourDirections = [
  [1, 0],
  [0, 1],
  [-1, 0],
  [0, -1],
];

const diagonalA = [
  [1, 1],
  [-1, -1],
];

const diagonalB = [
  [1, -1],
  [-1, 1],
];

const eightDirections = [...fourDirections, ...diagonalA, ...diagonalB];

let part1 = 0;

for (let line = 0; line < lines.length; line++) {
  for (let index = 0; index < lines[line].length; index++) {
    withNextDirection: for (const direction of eightDirections) {
      for (let wordIndex = 0; wordIndex < WORD.length; wordIndex++) {
        const searchLine = line + direction[0] * wordIndex;
        const searchIndex = index + direction[1] * wordIndex;
        if (lines[searchLine]?.[searchIndex] !== WORD[wordIndex]) {
          continue withNextDirection;
        }
      }
      part1++;
    }
  }
}

console.log(`Part 1: ${part1}`);

let part2 = 0;

for (let line = 0; line < lines.length; line++) {
  for (let index = 0; index < lines[line].length; index++) {
    if (lines[line][index] !== "A") continue;
    const isX = [diagonalA, diagonalB].every(
      (diagonalDirections) =>
        diagonalDirections
          .map(([dl, di]) => lines[line + dl]?.[index + di])
          .sort()
          .join("") === "MS"
    );
    if (isX) part2++;
  }
}

console.log(`Part 2: ${part2}`);
