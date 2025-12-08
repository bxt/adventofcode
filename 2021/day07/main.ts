#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import { minMax, range, sum } from "../../2020/utils.ts";

const parseInput = (string: string): number[] => {
  return string.trim().split(",").map((s) => parseInt(s, 10));
};

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

type FuelConsumption = (distance: number) => number;

const assumedFuelConsumption: FuelConsumption = (distance) => distance;

const findMinimumFuelConsumption = (
  input: number[],
  fuelConsumption: FuelConsumption,
): number => {
  const [from, to] = minMax(input);
  const targets = range(to - from).map((i) => i + from);
  const totalFuelConsumptions = (target: number) =>
    sum(input.map((n) => fuelConsumption(Math.abs(n - target))));
  return Math.min(...targets.map(totalFuelConsumptions));
};

const part1 = (input: number[]): number => {
  return findMinimumFuelConsumption(input, assumedFuelConsumption);
};

const example = parseInput(`16,1,2,0,4,2,7,1,2,14`);

assertEquals(part1(example), 37);

console.log("Result part 1: " + part1(input));

export const actualFuelConsumption: FuelConsumption = (distance) => {
  if (distance === 0) return 0;
  return distance * (distance + 1) / 2;
};

const part2 = (input: number[]): number => {
  return findMinimumFuelConsumption(input, actualFuelConsumption);
};

assertEquals(part2(example), 168);

console.log("Result part 2: " + part2(input));
