#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";

type BusLines = (number | null)[];

type Config = { time: number; busLines: BusLines };

const parseBusLines = (
  busLinesString: string,
): BusLines =>
  busLinesString.trim().split(",").map(
    (busLineString) => busLineString === "x" ? null : Number(busLineString),
  );

const parseInput = (
  string: string,
): Config => {
  const [timeString, busLinesString] = string.trim().split(/[\n ]+/);

  const busLines = parseBusLines(busLinesString);
  return { time: Number(timeString), busLines };
};

const example = parseInput(`
  939
  7,13,x,x,59,x,31,19
`);

assertEquals(example.time, 939);
assertEquals(example.busLines, [7, 13, null, null, 59, null, 31, 19]);

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

const minBy = <T>(array: T[], f: (e: T) => number) =>
  array.reduce((a, b) => f(a) < f(b) ? a : b);

const part1 = (config: Config): number => {
  const { time, busLines } = config;
  const arrivals = (busLines.filter((b) => b) as number[]).map((busLine) => ({
    busLine,
    nextArrivalIn: busLine - (time % busLine),
  }));
  const nextArrival = minBy(arrivals, (a) => a.nextArrivalIn);
  return nextArrival.busLine * nextArrival.nextArrivalIn;
};

assertEquals(part1(example), 295);

console.log("Result part 1: " + part1(inputParsed));

/**
 * Calculate modulus taking into account sign conversion like the math people do them.
 */
const mathModulus = (a: number, b: number) => {
  return ((a % b) + Math.abs(b)) % Math.abs(b);
};

assertEquals(mathModulus(7, 3), 1);
assertEquals(mathModulus(6, 3), 0);
assertEquals(mathModulus(3, 6), 3);
assertEquals(mathModulus(5, -22), 5);
assertEquals(mathModulus(-22, 5), 3);

/**
 * For two numbers x, y, return gcd and u, v such that ux + vy = gcd,
 * aka a linear combination of the gcd.
 */
const euklid = (x: number, y: number) => {
  let a = x;
  let b = y;
  let q = 1;
  let u = 1;
  let s = 0;
  let v = 0;
  let t = 1;
  while (b !== 0) {
    const r = mathModulus(a, b);
    q = (a - r) / b;
    a = b;
    b = r;
    [s, t, u, v] = [u - q * s, v - q * t, s, t];
  }

  return { gcd: a, u, v };
};

assertEquals(euklid(99, 78), { gcd: 3, u: -11, v: 14 });
assertEquals(euklid(78, 99), { gcd: 3, u: 14, v: -11 });
assertEquals(euklid(5, -22), { gcd: 1, u: 9, v: 2 });
assertEquals(euklid(499, 23), { gcd: 1, u: -10, v: 217 });
assertEquals(euklid(499, -23), { gcd: 1, u: -10, v: -217 });
assertEquals(euklid(-499, 23), { gcd: 1, u: 10, v: 217 });
assertEquals(euklid(23, 499), { gcd: 1, u: 217, v: -10 });
assertEquals(euklid(23, -499), { gcd: 1, u: 217, v: 10 });
assertEquals(euklid(-23, 499), { gcd: 1, u: -217, v: -10 });
assertEquals(euklid(449, 23), { gcd: 1, u: 2, v: -39 });
assertEquals(euklid(449, -23), { gcd: 1, u: 2, v: 39 });
assertEquals(euklid(-449, 23), { gcd: 1, u: -2, v: -39 });
assertEquals(euklid(23, 449), { gcd: 1, u: -39, v: 2 });
assertEquals(euklid(23, -449), { gcd: 1, u: -39, v: -2 });
assertEquals(euklid(-23, 449), { gcd: 1, u: 39, v: 2 });

type DiophantSoltuion = {
  xConst: number;
  xFactor: number;
  yConst: number;
  yFactor: number;
};

/**
 * For an equation like "ax + by = offset" returns parameters for solution
 * equations like "x = xFactor * t + xConst" and "y = yFactor * t + yConst"
 */
const diophant = (a: number, b: number, offset: number): DiophantSoltuion => {
  const { gcd, u, v } = euklid(a, b);
  // Seems all bus lines are prime anyways, but hey:
  if (offset % gcd !== 0) throw new Error("offset not divisible by gcd");
  const scale = offset / gcd;

  let xFactor = b / gcd;
  let yFactor = -a / gcd;

  // Optimize the factors:
  if (xFactor < 0 && yFactor < 0) {
    xFactor = -xFactor;
    yFactor = -yFactor;
  }
  const optimizeFactor = Math.floor(scale * u / xFactor);

  const xConst = scale * u - optimizeFactor * xFactor;
  const yConst = scale * v - optimizeFactor * yFactor;

  return { xConst, xFactor, yConst, yFactor };
};

assertEquals(
  diophant(6, 10, 100),
  { xConst: 0, xFactor: 5, yConst: 10, yFactor: -3 },
);
assertEquals(
  diophant(10, 6, 100),
  { xConst: 1, xFactor: 3, yConst: 15, yFactor: -5 },
);
assertEquals(
  diophant(23, -449, 394 - 2),
  { xConst: 427, xFactor: 449, yConst: 21, yFactor: 23 },
);
assertEquals(
  diophant(23, -41, -13),
  { xConst: 3, xFactor: 41, yConst: 2, yFactor: 23 },
);
assertEquals(
  diophant(23, -41, -13),
  { xConst: 3, xFactor: 41, yConst: 2, yFactor: 23 },
);

const recurse = (previous: DiophantSoltuion[]): number => {
  const processedPairs = [];
  for (let i = 0; i < previous.length - 1; i++) {
    processedPairs.push(
      diophant(
        previous[i].yFactor,
        -previous[i + 1].xFactor,
        previous[i + 1].xConst - previous[i].yConst,
      ),
    );
  }

  if (processedPairs.length === 1) {
    return processedPairs[0].yConst; // evaluate for t = 0
  }

  const previousYConst = recurse(processedPairs);

  const lastEquation = processedPairs[processedPairs.length - 1];

  return lastEquation.yConst + previousYConst * lastEquation.yFactor; // put into equation
};

const solve = (numbers: number[], offsets: number[]): number => {
  const processedPairs = [];
  for (let i = 0; i < numbers.length - 1; i++) {
    processedPairs.push(
      diophant(numbers[i], -numbers[i + 1], offsets[i] - offsets[i + 1]),
    );
  }

  const previousYConst = recurse(processedPairs);

  const lastEquation = processedPairs[processedPairs.length - 1];

  const result = lastEquation.yConst + previousYConst * lastEquation.yFactor; // put into equation

  return result * numbers[numbers.length - 1] - offsets[numbers.length - 1];
};

const part2 = (busLines: BusLines): number => {
  const indexedBusLines = busLines.map((b, i) =>
    b === null ? null : [b, i] as const
  );
  const filteredIndexedBusLines =
    (indexedBusLines.filter((b) => b) as [number, number][]);

  const numbers = filteredIndexedBusLines.map(([a, _]) => a);
  const offsets = filteredIndexedBusLines.map(([_, b]) => b);
  return solve(numbers, offsets);
};

assertEquals(part2(parseBusLines("17,x,13,19")), 3417);

assertEquals(part2(example.busLines), 1068781);

console.log("Result part 2: " + part2(inputParsed.busLines));
