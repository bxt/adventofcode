// run with `deno run --allow-read=input.txt main.ts`

type Position = readonly [number, number];

const WALL = "#";
const START = "S";
const END = "E";

const add = ([x1, y1]: Position, [x2, y2]: Position): Position => {
  return [x1 + x2, y1 + y2];
};

const directions: Position[] = [
  [1, 0],
  [0, 1],
  [-1, 0],
  [0, -1],
];

const file = await Deno.readTextFile("input.txt");

const field = file
  .split("\n")
  .filter((row) => row.trim())
  .map((row) => row.split(""));

let startPosition: Position | null = null;

for (let y = 0; y < field.length; y++) {
  for (let x = 0; x < field[y].length; x++) {
    if (field[y][x] === START) {
      startPosition = [x, y];
    }
  }
}
if (!startPosition) throw new Error("Start not found?");

const trackPositions = [startPosition];

const distances = new Map<string, number>();

distances.set(startPosition.toString(), 0);

for (let i = 0; i < trackPositions.length; i++) {
  const position = trackPositions[i];
  const distance = distances.get(position.toString())!;

  for (const direction of Object.values(directions)) {
    const nextPosition = add(position, direction);
    const nextValue = field[nextPosition[1]]?.[nextPosition[0]];

    if (!nextValue) continue;
    if (nextValue === WALL) continue;
    if (distances.has(nextPosition.toString())) continue;

    distances.set(nextPosition.toString(), distance + 1);

    if (nextValue === END) continue;
    trackPositions.push(nextPosition);
  }
}

let count = 0;

for (const trackPosition of trackPositions) {
  const distance = distances.get(trackPosition.toString());
  if (distance === undefined) throw new Error('No distance?');

  for (const direction1 of Object.values(directions)) {
    for (const direction2 of Object.values(directions)) {
      const endPosition = add(add(trackPosition, direction1), direction2);
      const endDistance = distances.get(endPosition.toString());
      if (!endDistance) continue;

      const saved = endDistance - distance - 2;
      if (saved >= 100) count++;
    }
  }
}

console.log(`Part 1: ${count}`);
