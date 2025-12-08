#!/usr/bin/env deno run --allow-read=input.txt

type Position = readonly [number, number];

const parse = (input: string): number[][] => {
  return input
    .trim()
    .split("\n")
    .map((line) =>
      line
        .trim()
        .split("")
        .map((s) => parseInt(s, 10))
    );
};

const add = ([x1, y1]: Position, [x2, y2]: Position): Position => {
  return [x1 + x2, y1 + y2];
};

const fourDirections = [
  [1, 0],
  [0, 1],
  [-1, 0],
  [0, -1],
] as const;

const file = await Deno.readTextFile("input.txt");

const parsedInput = parse(file);

const get = ([x, y]: Position): number | undefined => {
  return parsedInput[y]?.[x];
};

const equals = ([x1, y1]: Position, [x2, y2]: Position): boolean => {
  return x1 === x2 && y1 === y2;
};

let countPart1 = 0;
let countPart2 = 0;

for (let y = 0; y < parsedInput.length; y++) {
  for (let x = 0; x < parsedInput[y].length; x++) {
    const position: Position = [x, y];
    let frontier = [position];
    if (get(position) !== 0) continue;

    for (let distance = 0; distance < 9; distance++) {
      frontier = frontier.flatMap((position) =>
        fourDirections.map((direction) => add(position, direction))
      );
      frontier = frontier.filter((position) => get(position) === distance + 1);
    }

    countPart2 += frontier.length;
    countPart1 += frontier.filter((position, index) => frontier.findLastIndex((p) => equals(p, position)) === index).length;
  }
}

console.log(`Part 1: ${countPart1}`);
console.log(`Part 2: ${countPart2}`);
