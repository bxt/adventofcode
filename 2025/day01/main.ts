#!/usr/bin/env deno run --allow-read=input.txt

const file = await Deno.readTextFile("input.txt");

const regex = /([LR])(\d+)/g;

const instructions = Array.from(
  file.matchAll(regex).map((match) => {
    const [_, directionString, distanceString] = match;
    if (directionString !== "L" && directionString !== "R") {
      throw new Error(`Unknown direction: ${directionString}`);
    }
    return {
      direction: { "L": -1, "R": 1 }[directionString],
      distance: parseInt(distanceString, 10),
    };
  }),
);

const wheelSize = 100;

let currentPosition = 50;
let part1 = 0;
let part2 = 0;

for (const { direction, distance } of instructions) {
  for (let i = 0; i < distance; i++) {
    currentPosition += direction;
    if (currentPosition < 0) {
      currentPosition += wheelSize;
    } else if (currentPosition >= wheelSize) {
      currentPosition -= wheelSize;
    }
    if (currentPosition === 0) {
      part2++;
    }
  }
  if (currentPosition === 0) {
    part1++;
  }
}

console.log(`Part 1: ${part1}`);
console.log(`Part 2: ${part2}`);
