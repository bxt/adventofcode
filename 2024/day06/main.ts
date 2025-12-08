#!/usr/bin/env deno run --allow-read=input.txt

const obstacle = "#";
const guard = "^";

const parse = (input: string): string[] => {
  return input
    .trim()
    .split("\n")
    .map((line) => line.trim());
};

const file = await Deno.readTextFile("input.txt");

const parsedInput = parse(file);

let initialGuardPosition: readonly [number, number] | undefined;
for (let y = 0; y < parsedInput.length; y++) {
  const x = parsedInput[y].indexOf(guard);
  if (x !== -1) {
    initialGuardPosition = [x, y] as const;
    break;
  }
}
if (initialGuardPosition === undefined) throw new Error("Guard not found");

const directions = ["up", "right", "down", "left"] as const;
type Direction = (typeof directions)[number];

const walk = (input: string[]): "loop" | string[] => {
  const seen: Record<string, Direction[]> = {};

  let directionIndex = 0;
  let guardPosition = initialGuardPosition;

  while (true) {
    {
      const guardPositionString = guardPosition.toString();
      seen[guardPositionString] ??= [];
      if (seen[guardPositionString].includes(directions[directionIndex])) {
        return "loop";
      }
      seen[guardPositionString].push(directions[directionIndex]);
    }

    const [x, y] = guardPosition;
    const possibleNewPositions = {
      up: [x, y - 1],
      down: [x, y + 1],
      left: [x - 1, y],
      right: [x + 1, y],
    } as const;
    const newPosition = possibleNewPositions[directions[directionIndex]];

    const [newX, newY] = newPosition;
    const newTile = input[newY]?.[newX];

    if (newTile === undefined) break; // reached the end of the map

    if (newTile === obstacle) {
      directionIndex = (directionIndex + 1) % directions.length;
    } else {
      guardPosition = newPosition;
    }
  }

  return Object.keys(seen);
};

const distinctPositions = walk(parsedInput);

if (distinctPositions === "loop") throw new Error("Loop found");

console.log(`Part 1: ${distinctPositions.length}`);

let possibleObstaclePositions = 0;

for (const obstaclePosition of distinctPositions) {
  if (obstaclePosition === initialGuardPosition.toString()) continue;
  const [x, y] = obstaclePosition.split(",").map((s) => parseInt(s, 10));
  const modifiedInput = structuredClone(parsedInput);
  modifiedInput[y] =
    modifiedInput[y].substring(0, x) +
    obstacle +
    modifiedInput[y].substring(x + 1);

  if (walk(modifiedInput) === "loop") {
    possibleObstaclePositions++;
  }
}

console.log(`Part 2: ${possibleObstaclePositions}`);
