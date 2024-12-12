// run with `deno run --allow-read=input.txt main.ts`

type Position = readonly [number, number];

const parse = (input: string): string[] => {
  return input
    .trim()
    .split("\n")
    .map((line) => line.trim());
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

const get = ([x, y]: Position): string | undefined => {
  return parsedInput[y]?.[x];
};

const seen = new Set<string>();

let result = 0;
let currentArea = 0;
let currentBoundary = 0;

const process = (position: Position): void => {
  const positionString = position.toString();
  if (seen.has(positionString)) return;
  seen.add(positionString);

  currentArea++;

  const value = get(position);

  for (const direction of fourDirections) {
    const nextPosition = add(position, direction);
    const nextValue = get(nextPosition);
    if (nextValue === value) {
      process(nextPosition);
    } else {
      currentBoundary++;
    }
  }
};

parsedInput.forEach((line, y) => {
  for (let x = 0; x < line.length; x++) {
    process([x, y]);
    result += currentArea * currentBoundary;
    currentArea = 0;
    currentBoundary = 0;
  }
});

console.log(`Part 1: ${result}`);
