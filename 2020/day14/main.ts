#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import { ensureElementOf, matchGroups, sum } from "../utils.ts";

const one = "1";
const zero = "0";

const binaryDigits: BinaryDigit[] = [one, zero];

type BinaryDigit = typeof one | typeof zero;

type MaybeBinaryDigit = BinaryDigit | null;

type Mask = MaybeBinaryDigit[];

type SetMemoryInstruction = {
  kind: "set-memory";
  value: number;
  address: number;
};

type Instruction = { kind: "set-mask"; mask: Mask } | SetMemoryInstruction;

type Memory = { values: Record<number, number>; mask: Mask };

type InstructionEvaluator = (
  memory: Memory,
  instruction: Instruction,
) => Memory;

const parseInput = (
  string: string,
): Instruction[] =>
  string.trim().split(/\n */).map((line) => {
    if (line.startsWith("mask")) {
      const { maskString } = matchGroups(/mask = (?<maskString>[01X]{36})/)(
        line,
      );
      return {
        kind: "set-mask",
        mask: maskString.split("").map((x) =>
          x === "X" ? null : x === "0" ? zero : one
        ),
      };
    } else {
      const { addressString, valueString } = matchGroups(
        /mem\[(?<addressString>\d+)\] = (?<valueString>\d+)/,
      )(
        line,
      );
      return {
        address: Number(addressString),
        kind: "set-memory",
        value: Number(valueString),
      };
    }
  });

const example = parseInput(`
  mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
  mem[8] = 11
  mem[7] = 101
  mem[8] = 0
`);

assertEquals(example[0].kind, "set-mask");
if (example[0].kind === "set-mask") {
  assertEquals(example[0].mask.length, 36);
  assertEquals(example[0].mask[0], null);
  assertEquals(example[0].mask[34], zero);
  assertEquals(example[0].mask[29], one);
}
assertEquals(example[1], { kind: "set-memory", value: 11, address: 8 });
assertEquals(example.length, 4);

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

const convertToBinary = (value: number): BinaryDigit[] =>
  value.toString(2).split("").map((d) => ensureElementOf(d, binaryDigits))
    .reverse();

const convertFromBinary = (binaryDigits: BinaryDigit[]): number =>
  parseInt(binaryDigits.join(""), 2);

const applyMask = (value: number, mask: Mask): number => {
  const valueBinary = convertToBinary(value);
  const mapped = mask.map((maskDigit, iBackwards) => {
    const i = 35 - iBackwards;
    if (maskDigit !== null) return maskDigit;
    if (i >= valueBinary.length) return zero;
    return valueBinary[i];
  });
  return convertFromBinary(mapped);
};

const runInstructionPart1 = (
  memory: Memory,
  instruction: Instruction,
): Memory => {
  switch (instruction.kind) {
    case "set-mask": {
      return { ...memory, mask: instruction.mask };
    }
    case "set-memory": {
      return {
        ...memory,
        values: {
          ...memory.values,
          [instruction.address]: applyMask(instruction.value, memory.mask),
        },
      };
    }
  }
};

const runInstructions = (
  instructions: Instruction[],
  instructionEvaluator: InstructionEvaluator,
): number => {
  const initalMemory: Memory = { values: {}, mask: [] };
  const finalMemory = instructions.reduce(
    instructionEvaluator,
    initalMemory,
  );
  return sum(Object.values(finalMemory.values).filter((v) => v));
};

const part1 = (instructions: Instruction[]): number =>
  runInstructions(instructions, runInstructionPart1);

assertEquals(part1(example), 165);

console.log("Result part 1: " + part1(inputParsed));

const example2 = parseInput(`
  mask = 000000000000000000000000000000X1001X
  mem[42] = 100
  mask = 00000000000000000000000000000000X0XX
  mem[26] = 1
`);

// Oh man, a list monad would be nice...
const prependToAll = <T>(arrays: T[][], elements: T[]) =>
  elements.flatMap((e) => arrays.map((a) => [e, ...a]));

const spreadMask = (address: number, mask: Mask): number[] => {
  const valueBinary = convertToBinary(address);
  const mappeds = mask.reduce((digitsSoFar, maskDigit, iBackwards) => {
    const i = 35 - iBackwards;
    let newDigits: BinaryDigit[] | null = null;
    switch (maskDigit) {
      case "0":
        newDigits = [i >= valueBinary.length ? zero : valueBinary[i]];
        break;
      case "1":
        newDigits = [one];
        break;
      case null:
        newDigits = binaryDigits;
        break;
    }
    return prependToAll(digitsSoFar, newDigits);
  }, [[]] as BinaryDigit[][]);

  return mappeds.map(convertFromBinary);
};

const runInstructionPart2 = (
  memory: Memory,
  instruction: Instruction,
): Memory => {
  switch (instruction.kind) {
    case "set-mask": {
      return { ...memory, mask: instruction.mask };
    }
    case "set-memory": {
      const addresses = spreadMask(instruction.address, memory.mask);

      const values = { ...memory.values };
      addresses.forEach((address) => {
        values[address] = instruction.value;
      });

      return { ...memory, values };
    }
  }
};

const part2 = (instructions: Instruction[]): number =>
  runInstructions(instructions, runInstructionPart2);

assertEquals(part2(example2), 208);

console.log("Result part 2: " + part2(inputParsed));
