#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { ensureElementOf, matchGroups, sum } from "../../2020/utils.ts";

const cucumbers = [">", "v"] as const;
type Cucumber = typeof cucumbers[number];
const empty = ".";
type Element = Cucumber | typeof empty;
const elements: Element[] = [empty, ...cucumbers];

function parseInput(string: string): Element[][] {
  const lines = string.trim().split("\n");
  return lines.map((line) => {
    return line.trim().split("").map((e) => ensureElementOf(e, elements));
  });
}

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

function part1(input: Element[][]): number {
  let hasAnyMoved = true;
  let steps = 0;
  let state = input;

  while (hasAnyMoved && steps < 1000) {
    console.log(steps, ":");
    console.log(state.map((l) => l.join("")).join("\n"));
    console.log("-------");

    steps++;
    hasAnyMoved = false;
    {
      const nextState: Element[][] = state.map((l) => [...l]);

      for (let y = state.length - 1; y >= 0; y--) {
        for (let x = state[y].length; x >= 0; x--) {
          const nextX = (x + 1) % state[y].length;
          if (state[y][x] === ">") {
            if (state[y][nextX] === empty) {
              nextState[y][x] = empty;
              nextState[y][nextX] = ">";
              hasAnyMoved = true;
            } else {
              nextState[y][x] = ">";
            }
          }
        }
      }
      state = nextState;
    }
    {
      const nextState: Element[][] = state.map((l) => [...l]);

      for (let y = state.length - 1; y >= 0; y--) {
        const nextY = (y + 1) % state.length;
        for (let x = state[y].length; x >= 0; x--) {
          if (state[y][x] === "v") {
            if (state[nextY][x] === empty) {
              nextState[y][x] = empty;
              nextState[nextY][x] = "v";
              hasAnyMoved = true;
            } else {
              nextState[y][x] = "v";
            }
          }
        }
      }

      state = nextState;
    }
  }

  console.log(steps, ":");
  console.log(state.map((l) => l.join("")).join("\n"));

  return steps;
}

const example = parseInput(`
  v...>>.vv>
  .vv>>.vv..
  >>.>v>...v
  >>v>>.>.v.
  v>v.vv.v..
  >.>>..v...
  .vv..>.>v.
  v.v..>>v.v
  ....v..v.>
`);

assertEquals(part1(example), 58);

console.log("Result part 1: " + part1(input));
