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

const EAST = [1, 0] as const;

const fourDirections = [
  EAST,
  [0, 1],
  [-1, 0],
  [0, -1],
] as const;

const file = await Deno.readTextFile("input.txt");

const parsedInput = parse(file);

const get = ([x, y]: Position): string | undefined => {
  return parsedInput[y]?.[x];
};

const equals = ([x1, y1]: Position, [x2, y2]: Position): boolean => {
  return x1 === x2 && y1 === y2;
};

const startPosition: Position = [1,parsedInput.length-2];
if (get(startPosition) !== 'S') throw new Error("No start?");

const endPosition: Position = [parsedInput[0].length-2, 1];
if (get(endPosition) !== 'E') throw new Error("No end?");

const knownDistances: Record<string, number> = {};

const saveKnownDistance = (position: Position, direction: Position, distance: number): void => {
  knownDistances[`${position.toString()}-${direction.toString()}`] = distance;
}

const getKnownDistance = (position: Position, direction: Position): number => {
  return knownDistances[`${position.toString()}-${direction.toString()}`] ?? Infinity;
}

const openEnds: Array<[number, Position, Position, Record<string, boolean>]> = [
  [0, startPosition, EAST, {[startPosition.toString()]: true}],
];

saveKnownDistance(startPosition, EAST, 0);

let bestScore = undefined;
let visitedBestPath = {};

while(openEnds.length > 0) {
  const [score, position, direction, visited] = openEnds.shift()!;

  if (bestScore !== undefined && score > bestScore) break;

  if (equals(position, endPosition)) {
    bestScore = score;
    visitedBestPath = {...visitedBestPath, ...visited};
    continue;
  }

  for (const newDirection of fourDirections) {
    let newScore, newPosition, newVisited;

    if (equals(newDirection, direction)) {
      newScore = score + 1;
      newPosition = add(position, direction);
      const value = get(newPosition);
      if (value !== '.' && value !== 'E') continue;
      newVisited = {...visited, [newPosition.toString()]: true};
    } else {
      newScore = score + 1000;
      newPosition = position;
      newVisited = visited;
    }

    const knownDistance = getKnownDistance(newPosition, newDirection);
    if (newScore <= knownDistance) {
      saveKnownDistance(newPosition, newDirection, newScore);
      openEnds.push([newScore, newPosition, newDirection, newVisited]);
      openEnds.sort((a, b) => a[0] - b[0]);
    }
  }
}

console.log(`Part 1: ${bestScore}`);
console.log(`Part 2: ${Object.keys(visitedBestPath).length}`);
