#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";
import { matchGroups, sum } from "../utils.ts";

type Mask = (0 | 1 | null)[];

type Instruction = { kind: "set-mask"; mask: Mask } | {
  kind: "set-memory";
  value: number;
  address: number;
};

type Memory = { values: Record<number, number>; mask: Mask };

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
          x === "X" ? null : x === "0" ? 0 : 1
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
  assertEquals(example[0].mask[34], 0);
  assertEquals(example[0].mask[29], 1);
}
assertEquals(example[1], { kind: "set-memory", value: 11, address: 8 });
assertEquals(example.length, 4);

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

const applyMask = (value: number, mask: Mask): number => {
  const valueBinary = value.toString(2).split("").map(Number).reverse();
  const mapped = mask.map((maskDigit, iBackwards) => {
    const i = 35 - iBackwards;
    if (maskDigit !== null) return maskDigit;
    if (i >= valueBinary.length) return 0;
    return valueBinary[i];
  });
  const result = mapped.join("");
  const resultInt = parseInt(result, 2);
  return resultInt;
};

const runInstruction = (memory: Memory, instruction: Instruction): Memory => {
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

const part1 = (instructions: Instruction[]): number => {
  const initalMemory: Memory = { values: {}, mask: [] };
  const finalMemory = instructions.reduce(runInstruction, initalMemory);
  return sum(Object.values(finalMemory.values).filter((v) => v));
};

assertEquals(part1(example), 165);

console.log("Result part 1: " + part1(inputParsed));
