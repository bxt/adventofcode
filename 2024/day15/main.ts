// run with `deno run --allow-read=input.txt main.ts`

type Position = readonly [number, number];

const ROBOT = "@";
const FREE = ".";
const WALL = "#";
const BOX = "O";

const add = ([x1, y1]: Position, [x2, y2]: Position): Position => {
  return [x1 + x2, y1 + y2];
};

const directions: Record<string, Position> = {
  ">": [1, 0],
  v: [0, 1],
  "<": [-1, 0],
  "^": [0, -1],
};

const file = await Deno.readTextFile("input.txt");

const [fieldString, movesString] = file.split("\n\n");

const field = fieldString.split("\n").map((row) => row.split(""));

const get = ([x, y]: Position): string | undefined => field[y]?.[x];
const set = ([x, y]: Position, value: string): void => {
  field[y][x] = value;
};

const moves = movesString.split("").flatMap((move) => {
  const result = directions[move];
  if (!result) return [];
  return [result];
});

let robotPosition = ((): Position => {
  for (let y = 0; y < field.length; y++) {
    for (let x = 0; x < field[y].length; x++) {
      if (field[y][x] === ROBOT) {
        return [x, y];
      }
    }
  }
  throw new Error("Robot not found?");
})();

withNextMove: for (const move of moves) {
  const nextRobotPosition = add(robotPosition, move);

  if (get(nextRobotPosition) === FREE) {
    set(nextRobotPosition, ROBOT);
    set(robotPosition, FREE);
    robotPosition = nextRobotPosition;
    continue;
  }

  let nextBoxPosition = nextRobotPosition;
  let nextBoxPositionValue = get(nextBoxPosition);

  while (nextBoxPositionValue !== FREE) {
    if (nextBoxPositionValue === WALL) continue withNextMove;
    if (nextBoxPositionValue !== BOX) throw new Error("?");
    nextBoxPosition = add(nextBoxPosition, move);
    nextBoxPositionValue = get(nextBoxPosition);
  }

  set(nextRobotPosition, ROBOT);
  set(robotPosition, FREE);
  robotPosition = nextRobotPosition;

  set(nextBoxPosition, BOX);
}

let boxCoordinateSum = 0;

for (let y = 0; y < field.length; y++) {
  for (let x = 0; x < field[y].length; x++) {
    if (field[y][x] === BOX) {
      boxCoordinateSum += x + y * 100;
    }
  }
}

console.log(`Part 1: ${boxCoordinateSum}`);
