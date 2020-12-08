#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";

type Command = { command: "nop" | "acc" | "jmp"; number: number };

const parseInput = (string: string): Command[] =>
  [...string.matchAll(
    /\W*(?<command>.*?) (?<number>[+-]\d+)/g,
  )]
    .map(({ groups }) => {
      if (!groups) throw new Error("?");
      const { command, number } = groups;

      if (command !== "nop" && command !== "acc" && command !== "jmp") {
        throw new Error(command);
      }

      return { command, number: Number(number) };
    });

const example = parseInput(`
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
`);

assertEquals(
  example[0],
  {
    command: "nop",
    number: 0,
  },
);

assertEquals(
  example[1],
  {
    command: "acc",
    number: 1,
  },
);

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

const part1 = (commands: Command[]): number => {
  let acc = 0;
  const seen = new Set();
  let index = 0;
  while (!seen.has(index)) {
    seen.add(index);
    const { command, number } = commands[index];
    if (command === "nop") {
      index++;
    } else if (command === "acc") {
      acc += number;
      index++;
    } else if (command === "jmp") {
      index += number;
    }
  }
  return acc;
};

assertEquals(part1(example), 5);

console.log("Result part 1: " + part1(inputParsed));

const part2 = (commands: Command[]): number => {
  xx:
  for (let i = 0; i < commands.length; i++) {
    if (commands[i].command === "acc") continue xx;
    if (commands[i].command === "nop") commands[i].command = "jmp";
    else commands[i].command = "nop";

    let acc = 0;
    const seen = new Set();
    let index = 0;
    while (index < commands.length) {
      if (seen.has(index)) {
        if (commands[i].command === "nop") commands[i].command = "jmp";
        else commands[i].command = "nop";
        continue xx;
      }
      seen.add(index);
      const { command, number } = commands[index];
      if (command === "nop") {
        index++;
      } else if (command === "acc") {
        acc += number;
        index++;
      } else if (command === "jmp") {
        index += number;
      }
    }
    return acc;
  }
  throw new Error("hm.");
};

assertEquals(part2(example), 8);

console.log("Result part 2: " + part2(inputParsed));
