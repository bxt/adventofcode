#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { product, range } from "../utils.ts";

const parseInput = (string: string): number[] => {
  return string.trim().split("").map(Number);
};

const example = parseInput("389125467");

assertEquals(example, [3, 8, 9, 1, 2, 5, 4, 6, 7]);

const input = "137826495";

const parsedInput = parseInput(input);

const buildCupMap = (
  input: number[],
  length: number,
): { nextCups: number[]; start: number } => {
  // Transform to 0-based:
  const adjustedInput = input.map((n) => n - 1);

  // Start with continuous coups, each one goes to next one:
  const nextCups = range(length).map((n) => n + 1);

  // Apply our input:
  for (let i = 0; i < adjustedInput.length - 1; i++) {
    nextCups[adjustedInput[i]] = adjustedInput[i + 1];
  }

  if (length > adjustedInput.length) {
    // after input, continue with continuous cups:
    nextCups[adjustedInput[adjustedInput.length - 1]] = adjustedInput.length;
    // Last continuous cup goes to first cup input:
    nextCups[nextCups.length - 1] = adjustedInput[0];
  } else {
    // Last input cup goes to first cup input:
    nextCups[adjustedInput[adjustedInput.length - 1]] = adjustedInput[0];
  }

  return { nextCups, start: adjustedInput[0] };
};

assertEquals(buildCupMap([3, 2, 1], 3).start, 2);
assertEquals(buildCupMap([3, 2, 1], 3).nextCups, [2, 0, 1]);
assertEquals(buildCupMap([3, 2, 1], 5).nextCups, [3, 0, 1, 4, 2]);
assertEquals(buildCupMap([3, 2, 1], 5).nextCups, [3, 0, 1, 4, 2]);
assertEquals(
  buildCupMap([5, 8, 3, 7, 4, 1, 9, 2, 6], 9).nextCups,
  [8, 5, 6, 0, 7, 4, 3, 2, 1],
);

const buildOutput = (nextCups: number[], length: number): number[] => {
  const output = [];
  let cup = 0;
  for (let i = 0; i < length; i++) {
    cup = nextCups[cup];
    output.push(cup + 1);
  }
  return output;
};

assertEquals(
  buildOutput(buildCupMap([3, 2, 1], 5).nextCups, 5),
  [4, 5, 3, 2, 1],
);
assertEquals(
  buildOutput(buildCupMap([5, 8, 3, 7, 4, 1, 9, 2, 6], 9).nextCups, 9),
  [9, 2, 6, 5, 8, 3, 7, 4, 1],
);
assertEquals(
  buildOutput(buildCupMap([1], 1).nextCups, 5),
  [1, 1, 1, 1, 1],
);

const runMoves = ({ amount, nextCups, start }: {
  amount: number;
  nextCups: number[];
  start: number;
}): number[] => {
  let currentCup = start;

  for (let i = 0; i < amount; i++) {
    // figure ut pickUp:
    const pickUp = [];
    let iteratorCup = currentCup;
    for (let k = 0; k < 3; k++) {
      iteratorCup = nextCups[iteratorCup];
      pickUp.push(iteratorCup);
    }

    // cup after pickUp will be next current cup:
    const nextCurrentCup = nextCups[iteratorCup];

    // slice out the pickUps:
    nextCups[currentCup] = nextCurrentCup;

    // figure destination:
    let destination = currentCup - 1;
    while (destination < 0 || pickUp.includes(destination)) {
      if (destination < 0) {
        destination = nextCups.length - 1;
      } else {
        destination--;
      }
    }

    // Slice in the pickUps:
    const prevDestinationNextCup = nextCups[destination];
    nextCups[destination] = pickUp[0];
    nextCups[pickUp[pickUp.length - 1]] = prevDestinationNextCup;

    // Prepare for next iteration:
    currentCup = nextCurrentCup;
  }

  return nextCups;
};

const runGame = ({ cups, numberOfCups, amountOfMoves, outputSize }: {
  cups: number[];
  numberOfCups: number;
  amountOfMoves: number;
  outputSize: number;
}): number[] => {
  const { start, nextCups } = buildCupMap(cups, numberOfCups);
  runMoves({ nextCups, amount: amountOfMoves, start });
  return buildOutput(nextCups, outputSize);
};

const part1 = (cups: number[]): string =>
  runGame({
    cups,
    numberOfCups: cups.length,
    amountOfMoves: 100,
    outputSize: cups.length - 1,
  }).join("");

console.log("Result part 1: " + part1(parsedInput));

const part2 = (cups: number[]): number =>
  product(runGame({
    cups,
    numberOfCups: 1000000,
    amountOfMoves: 10000000,
    outputSize: 2,
  }));

assertEquals(part2(example), 149245887792);

console.log("Result part 2: " + part2(parsedInput));
