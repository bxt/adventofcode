// run with `deno run --allow-read=input.txt main.ts`
import { assertEquals } from "jsr:@std/assert";

const obstacle = "#";
const guard = "^";

const parse = (input: string): string[] => {
  return input.trim().split("\n").map(line => line.trim());
};

const file = await Deno.readTextFile("input.txt");

const parsedInput = parse(file);

const distinctPositions = new Set<string>();

// find guard position
let guardPosition: readonly [number, number] | undefined;
for (let y = 0; y < parsedInput.length; y++) {
  const x = parsedInput[y].indexOf(guard);
  if (x !== -1) {
    guardPosition = [x, y] as const;
    break;
  }
}

// If there is something directly in front of you, turn right 90 degrees.
// Otherwise, take a step forward.
// End loop if guard leaves area

const directions = ["up", "right", "down", "left"] as const;

let directionIndex = 0;

while (true) {
  if (guardPosition === undefined)   throw new Error("Guard not found");
  distinctPositions.add(guardPosition.toString());

  const [x, y] = guardPosition;
  const newPosition = ({
    up: [x, y - 1],
    down: [x, y + 1],
    left: [x - 1, y],
    right: [x + 1, y],
  } as const)[directions[directionIndex]];

  const [newX, newY] = newPosition;
  const newTile = parsedInput[newY]?.[newX];

  if (newTile === undefined) break;

  if (newTile === obstacle) {
    directionIndex = (directionIndex + 1) % directions.length;
  } else {
    guardPosition = newPosition;
  }
}

console.log(`Part 1: ${distinctPositions.size}`);