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
  [1, 0], // S
  [0, 1], // E
  [-1, 0], // N
  [0, -1], // W
] as const;

const file = await Deno.readTextFile("input.txt");

const parsedInput = parse(file);

const get = ([x, y]: Position): string | undefined => {
  return parsedInput[y]?.[x];
};

const seen = new Set<string>();

let resultPart1 = 0;
let resultPart2 = 0;
let currentArea = 0;
let currentBoundary = 0;
let currentSides = 0;

const process = (position: Position): void => {
  const positionString = position.toString();
  if (seen.has(positionString)) return;
  seen.add(positionString);

  currentArea++;

  const value = get(position);

  const neighbors = fourDirections.map((direction) => add(position, direction));
  const matches = neighbors.map((neighbor) => get(neighbor) === value);

  for (let i = 0; i < 4; i++) {
    const nextI = (i + 1) % 4;
    if (matches[i]) {
      process(neighbors[i]);

      if (matches[nextI]) { // possible convex corner
        const diagonal = add(neighbors[i], fourDirections[nextI]);
        if (get(diagonal) !== value) {
          currentSides++;
        }
      }
    } else {
      currentBoundary++;

      if (!matches[nextI]) {
        currentSides++; // concave corner
      }
    }
  }

};

parsedInput.forEach((line, y) => {
  for (let x = 0; x < line.length; x++) {
    process([x, y]);
    resultPart1 += currentArea * currentBoundary;
    resultPart2 += currentArea * currentSides;
    currentArea = 0;
    currentBoundary = 0;
  }
});

console.log(`Part 1: ${resultPart1}`);
console.log(`Part 2: ${resultPart2}`);
