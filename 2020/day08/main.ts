#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";
import { ensureElementOf } from "../utils.ts";

const operations = ["nop", "acc", "jmp"] as const;
type Instruction = { operation: typeof operations[number]; argument: number };

const parseInput = (string: string): Instruction[] =>
  [...string.matchAll(
    /\W*(?<operation>.*?) (?<argument>[+-]\d+)/g,
  )]
    .map(({ groups }) => {
      if (!groups) throw new Error("?");
      const { operation, argument } = groups;

      return {
        operation: ensureElementOf(operation, operations),
        argument: Number(argument),
      };
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
    operation: "nop",
    argument: 0,
  },
);

assertEquals(
  example[1],
  {
    operation: "acc",
    argument: 1,
  },
);

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

type RunResult = {
  infiniteLoop: boolean;
  accumulatorValue: number;
};

const runInstructions = (instructions: Instruction[]): RunResult => {
  let accumulatorValue = 0;
  const seenInstructionPointers = new Set();
  let instructionPointer = 0;
  while (instructionPointer < instructions.length) {
    if (seenInstructionPointers.has(instructionPointer)) {
      return { infiniteLoop: true, accumulatorValue };
    }
    seenInstructionPointers.add(instructionPointer);
    const { operation, argument } = instructions[instructionPointer];
    if (operation === "nop") {
      instructionPointer++;
    } else if (operation === "acc") {
      accumulatorValue += argument;
      instructionPointer++;
    } else if (operation === "jmp") {
      instructionPointer += argument;
    }
  }
  return { infiniteLoop: false, accumulatorValue };
};

const part1 = (instructions: Instruction[]): number => {
  const { infiniteLoop, accumulatorValue } = runInstructions(instructions);
  if (!infiniteLoop) throw new Error("Expected inifnite loop!");
  return accumulatorValue;
};

assertEquals(part1(example), 5);

console.log("Result part 1: " + part1(inputParsed));

const switchInstructions = (
  instructions: Instruction[],
  index: number,
): void => {
  if (instructions[index].operation === "nop") {
    instructions[index].operation = "jmp";
  } else if (instructions[index].operation === "jmp") {
    instructions[index].operation = "nop";
  }
};

const part2 = (instructions: Instruction[]): number => {
  for (let i = 0; i < instructions.length; i++) {
    switchInstructions(instructions, i);
    const { infiniteLoop, accumulatorValue } = runInstructions(instructions);
    switchInstructions(instructions, i);

    if (!infiniteLoop) {
      return accumulatorValue;
    }
  }
  throw new Error("Always ran into infinite loops :/");
};

assertEquals(part2(example), 8);

console.log("Result part 2: " + part2(inputParsed));
