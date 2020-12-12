#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";
import {
  addCoords,
  Coord,
  ensureElementOf,
  manhattanNormCoord,
  matchGroups,
  rotateLeftNinetyDegreesCoord,
  scaleCoord,
} from "../utils.ts";

const actions = ["N", "S", "E", "W", "L", "R", "F"] as const;

type Instruction = { action: typeof actions[number]; number: number };

const E: Coord = [1, 0];
const N: Coord = [0, 1];
const W: Coord = [-1, 0];
const S: Coord = [0, -1];
const headings = { E, N, W, S } as const;

type Ship = { waypoint: Coord; position: Coord };

const parseInput = (string: string): Instruction[] =>
  string.trim().split(/[\n ]+/).map(
    matchGroups(/(?<letter>[A-Z])(?<number>\d+)/),
  )
    .map((groups) => ({
      action: ensureElementOf(groups.letter, actions),
      number: Number(groups.number),
    }));

const example = parseInput(`
  F10
  N3
  F7
  R90
  F11
`);

assertEquals(example[0], { action: "F", number: 10 });
assertEquals(example[1], { action: "N", number: 3 });
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

const walkOne = (moveShipOrWaypoint: "ship" | "waypoint") =>
  (ship: Ship, instruction: Instruction): Ship => {
    const { waypoint, position } = ship;
    const { action, number } = instruction;
    switch (action) {
      case "N":
      case "S":
      case "E":
      case "W": {
        const offset = headings[action];
        return moveShipOrWaypoint === "ship"
          ? {
            position: addCoords(position, scaleCoord(offset, number)),
            waypoint,
          }
          : {
            position,
            waypoint: addCoords(waypoint, scaleCoord(offset, number)),
          };
      }
      case "L":
      case "R": {
        const sign = action === "L" ? 1 : -1;
        const amount = degreesToAmount(number);
        const numberOfHeadings = Object.keys(headings).length;
        const leftTurns = (numberOfHeadings + amount * sign) % numberOfHeadings;
        return {
          waypoint: Array(leftTurns).fill(0).reduce(
            rotateLeftNinetyDegreesCoord,
            waypoint,
          ),
          position,
        };
      }
      case "F":
        return {
          waypoint,
          position: addCoords(
            position,
            scaleCoord(waypoint, number),
          ),
        };
    }
  };

const walkAllPart1 = (instructions: Instruction[]): Ship => {
  const initialShip: Ship = { waypoint: headings.E, position: [0, 0] };

  return instructions.reduce(walkOne("ship"), initialShip);
};

assertEquals(
  walkAllPart1(example),
  {
    waypoint: headings.S,
    position: addCoords(scaleCoord(headings.E, 17), scaleCoord(headings.S, 8)),
  },
);

const part1 = (instructions: Instruction[]): number =>
  manhattanNormCoord(walkAllPart1(instructions).position);

assertEquals(part1(example), 25);

console.log("Result part 1: " + part1(inputParsed));

const walkAllPart2 = (instructions: Instruction[]): Ship => {
  const initialShip: Ship = {
    waypoint: addCoords(scaleCoord(headings.E, 10), headings.N),
    position: [0, 0],
  };

  return instructions.reduce(walkOne("waypoint"), initialShip);
};

assertEquals(
  walkAllPart2(example),
  {
    waypoint: addCoords(scaleCoord(headings.E, 4), scaleCoord(headings.S, 10)),
    position: addCoords(
      scaleCoord(headings.E, 214),
      scaleCoord(headings.S, 72),
    ),
  },
);

const part2 = (instructions: Instruction[]): number =>
  manhattanNormCoord(walkAllPart2(instructions).position);

assertEquals(part2(example), 286);

console.log("Result part 2: " + part2(inputParsed));
