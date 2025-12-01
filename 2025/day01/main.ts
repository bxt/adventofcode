// run with `deno run --allow-read=input.txt main.ts`

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

let currentPosition = 50;
let part1 = 0;

for (const { direction, distance } of instructions) {
  currentPosition = (currentPosition + direction * distance) % 100;
  if (currentPosition === 0) {
    part1++;
  }
}

console.log(`Part 1: ${part1}`);
