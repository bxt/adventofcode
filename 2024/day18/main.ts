// run with `deno run --allow-read=input.txt main.ts`

type Position = readonly [number, number];

const MAX = 70;

const parse = (input: string): Position[] => {
  return input
    .trim()
    .split("\n")
    .map((line) => {
      const [x, y] = line.trim().split(",");
      return [parseInt(x, 10), parseInt(y, 10)];
    });
};

const add = ([x1, y1]: Position, [x2, y2]: Position): Position => {
  return [x1 + x2, y1 + y2];
};

const equals = ([x1, y1]: Position, [x2, y2]: Position): boolean => {
  return x1 === x2 && y1 === y2;
};

const isInBounds = ([x, y]: Position): boolean => {
  return x >= 0 && x <= MAX && y >= 0 && y <= MAX;
};

const fourDirections = [
  [1, 0],
  [0, 1],
  [-1, 0],
  [0, -1],
] as const;

const file = await Deno.readTextFile("input.txt");

const parsedInput = parse(file);

const startPosition: Position = [0, 0];

const endPosition: Position = [MAX, MAX];

const findPath = (blocked: Position[]): number | undefined => {
  let frontier = [startPosition];
  let steps = 0;

  const blockedSet = new Set(blocked.map((p) => p.toString()));

  const seen = new Set([startPosition.toString()]);

  while (true) {
    steps++;

    frontier = frontier
      .flatMap((position) =>
        fourDirections.map((direction) => add(position, direction))
      )
      .filter(isInBounds)
      .filter((position) => !blockedSet.has(position.toString()))
      .filter((position) => !seen.has(position.toString()));

    frontier = frontier.filter(
      (position, index) =>
        frontier.findLastIndex((p) => equals(p, position)) === index
    );

    frontier.forEach((position) => seen.add(position.toString()));

    if (frontier.some((p) => equals(p, endPosition))) break;
    if (frontier.length === 0) return undefined;
  }

  return steps;
};

const firstKilobyte = 1024;

console.log(`Part 1: ${findPath(parsedInput.slice(0, firstKilobyte))}`);

for (let i = firstKilobyte; i < parsedInput.length; i++) {
  const result = findPath(parsedInput.slice(0, i));
  if (result === undefined) {
    console.log(`Part 2: ${parsedInput[i - 1]}`);
    break;
  }
}
