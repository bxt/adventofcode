#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";
import { getColorEnabled } from "https://deno.land/std@0.79.0/fmt/colors.ts";
import {
  addCoords,
  Coord,
  ensureElementOf,
  manhattanNormCoord,
  matchGroups,
  rotateLeftNinetyDegreesCoord,
  scaleCoord,
} from "../utils.ts";

type Config = { time: number; busLines: (number | null)[] };

const parseInput = (
  string: string,
): Config => {
  const [timeString, busLinesString] = string.trim().split(/[\n ]+/);

  const busLines = busLinesString.trim().split(",").map(
    (busLineString) => busLineString === "x" ? null : Number(busLineString),
  );
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
