#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";

const sum = (numbers: number[]): number =>
  numbers.reduce((acc, number) => acc + number, 0);

type Coord = readonly [number, number];

const addCoords = (
  [x1, y1]: Coord,
  [x2, y2]: Coord,
): Coord => ([x1 + x2, y1 + y2]);

const scaleCoord = ([x, y]: Coord, s: number): Coord => ([x * s, y * s]);

const manhattanNormCoord = ([x, y]: Coord): number => Math.abs(x) + Math.abs(y);

type Instruction = { letter: string; number: number };

const east: Coord = [1, 0];
const north: Coord = [0, -1];
const west: Coord = [-1, 0];
const south: Coord = [0, 1];
const possibleHeadings: Coord[] = [east, north, west, south];

type Ship = { waypoint: Coord; position: Coord };

const parseInput = (string: string): Instruction[] =>
  string.trim().split(/[\n ]+/)
    .map((line) => {
      const groups = line.match(
        /(?<letter>[A-Z])(?<number>\d+)/,
      )?.groups;

      if (!groups) throw new Error(`Did not match: ${line}`);

      return {
        letter: groups.letter,
        number: Number(groups.number),
      };
    });

const example = parseInput(`
  F10
  N3
  F7
  R90
  F11
`);

assertEquals(example[0], { letter: "F", number: 10 });
assertEquals(example[1], { letter: "N", number: 3 });
assertEquals(example.length, 5);

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

const degreesToAmount = (degrees: number): number => {
  switch (degrees) {
    case 90:
      return 1;
    case 180:
      return 2;
    case 270:
      return 3;
  }

  throw new Error(`Unimplemented amount of degrees: ${degrees}`);
};

const walkOne = (
  ship: Ship,
  instruction: Instruction,
): Ship => {
  const { waypoint, position } = ship;
  const { letter, number } = instruction;
  if (letter === "N") {
    return {
      position,
      waypoint: addCoords(waypoint, scaleCoord(north, number)),
    };
  }
  if (letter === "S") {
    return {
      position,
      waypoint: addCoords(waypoint, scaleCoord(south, number)),
    };
  }
  if (letter === "E") {
    return {
      position,
      waypoint: addCoords(waypoint, scaleCoord(east, number)),
    };
  }
  if (letter === "W") {
    return {
      position,
      waypoint: addCoords(waypoint, scaleCoord(west, number)),
    };
  }
  if (letter === "L" || letter === "R") {
    const sign = letter === "L" ? 1 : -1;
    const amount = degreesToAmount(number);
    const leftTurns = (possibleHeadings.length + amount * sign) %
      possibleHeadings.length;
    return {
      waypoint: Array(leftTurns).fill(0).reduce(
        ([x, y], _) => ([y, -x]),
        waypoint,
      ),
      position,
    };
  }
  if (letter === "F") {
    return {
      waypoint,
      position: addCoords(
        position,
        scaleCoord(waypoint, number),
      ),
    };
  }

  throw new Error(`Unknown command: ${letter}`);
};

const walkAll = (instructions: Instruction[]): Ship => {
  const initialShip: Ship = {
    waypoint: addCoords(scaleCoord(east, 10), north),
    position: [0, 0],
  };

  return instructions.reduce(walkOne, initialShip);
};

assertEquals(
  walkAll(example),
  {
    waypoint: addCoords(scaleCoord(east, 4), scaleCoord(south, 10)),
    position: [214, 72],
  },
);

const part2 = (instructions: Instruction[]): number =>
  manhattanNormCoord(walkAll(instructions).position);

assertEquals(part2(example), 286);

console.log("Result part 2: " + part2(inputParsed));
