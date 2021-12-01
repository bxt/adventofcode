#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { matchGroups } from "../utils.ts";

type Entry = {
  from: number;
  letter: string;
  password: string;
  to: number;
};

const parseInput = (string: string): Entry[] =>
  string.trim().split(/\n/).map(
    matchGroups(/(?<from>\d+)-(?<to>\d+) (?<letter>[a-z]): (?<password>.*)/),
  ).map((groups) => ({
    from: Number(groups.from),
    letter: groups.letter,
    password: groups.password,
    to: Number(groups.to),
  }));

const text = await Deno.readTextFile("input.txt");

const entries = parseInput(text);

const part1 = (inputs: Entry[]) =>
  inputs.filter(({ from, letter, password, to }) => {
    const letterCount = password.split("").filter((l) => l === letter).length;
    return letterCount >= from && letterCount <= to;
  }).length;

const example = parseInput(`
  1-3 a: abcde
  1-3 b: cdefg
  2-9 c: ccccccccc
`);

assertEquals(part1(example), 2, "Example is wrong!");

console.log("Result part 1: " + part1(entries));

const part2 = (inputs: Entry[]) =>
  inputs.filter(({ from: position1, letter, password, to: position2 }) =>
    (password[position1 - 1] == letter) !=
      (password[position2 - 1] == letter)
  ).length;

assertEquals(part2(example), 1, "Example is wrong!");

console.log("Result part 2: " + part2(entries));
