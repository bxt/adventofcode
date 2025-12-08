#!/usr/bin/env deno run --allow-read=input.txt

type Position = readonly [number, number];

const ROBOT = "@";
const FREE = ".";
const WALL = "#";
const BOX = "O";
const BOX_LEFT = "[";
const BOX_RIGHT = "]";

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

const moves = movesString.split("").flatMap((move) => {
  const result = directions[move];
  if (!result) return [];
  return [result];
});

const solve = (field: string[][]): number => {
  const get = ([x, y]: Position): string | undefined => field[y]?.[x];
  const set = ([x, y]: Position, value: string): void => {
    field[y][x] = value;
  };

  let robotPosition: Position | null = null;

  for (let y = 0; y < field.length; y++) {
    for (let x = 0; x < field[y].length; x++) {
      if (field[y][x] === ROBOT) {
        robotPosition = [x, y];
      }
    }
  }
  if (!robotPosition) throw new Error("Robot not found?");

  withNextMove: for (const move of moves) {
    const placesFound = new Set<string>();
    const placesToMove: Position[] = [];

    const enqueue = (position: Position): void => {
      const positionString = position.toString();
      if (placesFound.has(positionString)) return;
      placesFound.add(positionString);
      placesToMove.push(position);
    };

    enqueue(robotPosition);

    for (let i = 0; i < placesToMove.length; i++) {
      const nextPlace = add(placesToMove[i], move);
      const nextPlaceValue = get(nextPlace);

      if (!nextPlaceValue) continue withNextMove;
      if (nextPlaceValue === WALL) continue withNextMove;
      if (nextPlaceValue === FREE) {
        // cool!
      } else if (nextPlaceValue === BOX) {
        enqueue(nextPlace);
      } else if (nextPlaceValue === BOX_LEFT) {
        enqueue(nextPlace);
        enqueue(add(nextPlace, directions[">"]));
      } else if (nextPlaceValue === BOX_RIGHT) {
        enqueue(nextPlace);
        enqueue(add(nextPlace, directions["<"]));
      } else {
        throw new Error(`Not sure what do do with "${nextPlaceValue}"`);
      }
    }

    robotPosition = add(robotPosition, move);

    for (let i = placesToMove.length - 1; i >= 0; i--) {
      const place = placesToMove[i];
      set(add(place, move), get(place)!);
      set(place, FREE);
    }
  }

  let boxCoordinateSum = 0;

  for (let y = 0; y < field.length; y++) {
    for (let x = 0; x < field[y].length; x++) {
      if (field[y][x] === BOX_LEFT || field[y][x] === BOX) {
        boxCoordinateSum += x + y * 100;
      }
    }
  }

  return boxCoordinateSum;
};

const fieldPart1 = fieldString.split("\n").map((row) => row.split(""));
console.log(`Part 1: ${solve(fieldPart1)}`);

const fieldPart2 = fieldString.split("\n").map((row) =>
  row.split("").flatMap((s) => {
    if (s === ROBOT) return [ROBOT, FREE];
    if (s === BOX) return [BOX_LEFT, BOX_RIGHT];
    return [s, s];
  })
);
console.log(`Part 2: ${solve(fieldPart2)}`);
