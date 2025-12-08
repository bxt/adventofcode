#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import { ensureElementOf } from "../../2020/utils.ts";

const horizontal = ">";
const vertical = "v";
const empty = ".";
const elements = [horizontal, vertical, empty] as const;
type Element = typeof elements[number];

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

  while (hasAnyMoved) {
    steps++;
    hasAnyMoved = false;
    {
      const nextState: Element[][] = state.map((l) => [...l]);

      for (let y = 0; y < state.length; y++) {
        for (let x = 0; x < state[y].length; x++) {
          const nextX = (x + 1) % state[y].length;
          if (state[y][x] === horizontal) {
            if (state[y][nextX] === empty) {
              nextState[y][x] = empty;
              nextState[y][nextX] = horizontal;
              hasAnyMoved = true;
            }
          }
        }
      }
      state = nextState;
    }
    {
      const nextState: Element[][] = state.map((l) => [...l]);

      for (let y = 0; y < state.length; y++) {
        const nextY = (y + 1) % state.length;
        for (let x = 0; x < state[y].length; x++) {
          if (state[y][x] === vertical) {
            if (state[nextY][x] === empty) {
              nextState[y][x] = empty;
              nextState[nextY][x] = vertical;
              hasAnyMoved = true;
            }
          }
        }
      }
      state = nextState;
    }
  }

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
