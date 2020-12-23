#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";

const parseInput = (string: string): number[] => {
  return string.trim().split("").map(Number);
};

const example = parseInput("389125467");

assertEquals(example, [3, 8, 9, 1, 2, 5, 4, 6, 7]);

const input = "137826495";

const parsedInput = parseInput(input);

const stringifyCups = (cups: number[]): string => cups.join("");

const rotateCups = (
  cups: number[],
  by: number,
): number[] => [...cups.slice(by), ...cups.slice(0, by)];

assertEquals(rotateCups([1, 2, 3, 4], 1), [2, 3, 4, 1]);
assertEquals(rotateCups([1, 2, 3, 4, 5], 2), [3, 4, 5, 1, 2]);
assertEquals(rotateCups([1, 2, 3, 4, 5], 0), [1, 2, 3, 4, 5]);

const buildOutput = (cups: number[]): string => {
  const [one, ...rest] = rotateCups(cups, cups.indexOf(1));
  assertEquals(one, 1);
  return stringifyCups(rest);
};

assertEquals(buildOutput([5, 8, 3, 7, 4, 1, 9, 2, 6]), "92658374");

const max = (numbers: number[]): number => {
  let max = -Infinity;
  for (let i = 0; i < numbers.length; i++) {
    if (numbers[i] > max) {
      max = numbers[i];
    }
  }
  return max;
};

const runMoves = (cups: number[], amount: number): number[] => {
  let arrangement = [...cups];
  for (let i = 0; i < amount; i++) {
    console.log(`-- move ${i + 1} --`);
    const iMod = i % arrangement.length;
    const current = arrangement[iMod];
    console.log(
      `cups: ${
        arrangement.map((c) => c === current ? `(${c})` : ` ${c} `).join("")
      }`,
    );
    let pickUp = arrangement.splice(iMod + 1, 3);
    const pickUpFromStartLength = 3 - pickUp.length;
    pickUp = [...pickUp, ...arrangement.splice(0, pickUpFromStartLength)];
    console.log(`pick up: ${pickUp.join(", ")}`);
    const lowerCups = arrangement.filter((c) => c < current);
    const destination = lowerCups.length ? max(lowerCups) : max(arrangement);
    console.log(`destination: ${destination}\n`);
    const destinationIndex = arrangement.indexOf(destination) + 1;
    arrangement = [
      ...arrangement.slice(0, destinationIndex),
      ...pickUp,
      ...arrangement.slice(destinationIndex),
    ];
    if (destinationIndex <= iMod) {
      arrangement = rotateCups(arrangement, 3);
    }
    arrangement = rotateCups(
      arrangement,
      arrangement.length - pickUpFromStartLength,
    );
  }
  return arrangement;
};

assertEquals(runMoves(example, 10), [5, 8, 3, 7, 4, 1, 9, 2, 6]);

const part1 = (cups: number[]): string => {
  return buildOutput(runMoves(cups, 100));
};

assertEquals(part1(example), "67384529");

console.log("Result part 1: " + part1(parsedInput));

const part2 = (cups: number[]): string => {
  return buildOutput(
    runMoves(
      Array(1000000 - cups.length).fill(null).map((_, i) =>
        i + cups.length + 1
      ),
      10000000,
    ),
  );
};

console.log("Result part 2: " + part2(parsedInput)); // will run ~4 days, I guess, lol
