#!/usr/bin/env deno run --allow-read

const TARGET = 2020;

const parseInput = (string) =>
  string.trim().split(/\n/).map((string) =>
    string.match(/(?<from>\d+)-(?<to>\d+) (?<letter>[a-z]): (?<pw>.*)/).groups
  );

const text = await Deno.readTextFile("input.txt");

const entries = parseInput(text);

const part1 = (inputs) => {
  return inputs.filter(input => {
    const letterCount = input.pw.split('').filter(x => x === input.letter).length;
    return letterCount >= input.from && letterCount <= input.to
  }).length
};

const example = parseInput(`
1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
`);

if (part1(example) !== 2) throw new Error("Example is wrong!");

console.log("Result part 1: " + part1(entries));
