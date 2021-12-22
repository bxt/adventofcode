#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { matchGroups, sum } from "../../2020/utils.ts";

const matchInput = matchGroups(
  /(?<state>on|off) x=(?<x1>-?\d+)..(?<x2>-?\d+),y=(?<y1>-?\d+)..(?<y2>-?\d+),z=(?<z1>-?\d+)..(?<z2>-?\d+)/,
);

type Cuboid = {
  state: boolean;
  x1: number;
  x2: number;
  y1: number;
  y2: number;
  z1: number;
  z2: number;
};

function parseInput(string: string): Cuboid[] {
  const lines = string.trim().split("\n");
  return lines.map((line) => {
    const { state, x1, x2, y1, y2, z1, z2 } = matchInput(line);
    return {
      state: state === "on",
      x1: parseInt(x1, 10),
      x2: parseInt(x2, 10),
      y1: parseInt(y1, 10),
      y2: parseInt(y2, 10),
      z1: parseInt(z1, 10),
      z2: parseInt(z2, 10),
    };
  });
}

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

function part1(input: Cuboid[]): number {
  const cubeStates: Record<string, boolean> = {};

  for (const cuboid of input) {
    const { state, x1, x2, y1, y2, z1, z2 } = cuboid;
    for (let x = Math.max(x1, -50); x <= Math.min(x2, 50); x++) {
      for (let y = Math.max(y1, -50); y <= Math.min(y2, 50); y++) {
        for (let z = Math.max(z1, -50); z <= Math.min(z2, 50); z++) {
          cubeStates[`${x},${y},${z}`] = state;
        }
      }
    }
  }

  return Object.values(cubeStates).filter((b) => b).length;
}

const example = parseInput(`
  on x=10..12,y=10..12,z=10..12
  on x=11..13,y=11..13,z=11..13
  off x=9..11,y=9..11,z=9..11
  on x=10..10,y=10..10,z=10..10
`);

assertEquals(part1(example), 39);

console.log("Result part 1: " + part1(input));
