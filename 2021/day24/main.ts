#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { matchGroups } from "../../2020/utils.ts";

const matchCodeSection = matchGroups(
  /mul x 0\nadd x z\nmod x 26\ndiv z (?<divZ>1|26)\nadd x (?<offset>-?[0-9]+)\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y (?<summand>-?[0-9]+)\nmul y x\nadd z y/,
);

function parseInput(input: string): [number[], number[]] {
  const blocks = input.split("inp w").map((s) => s.trim()).filter((s) => s);
  const parsedBlocks = blocks.map(matchCodeSection);

  return [
    parsedBlocks.map((b) => parseInt(b.offset, 10)),
    parsedBlocks.map((b) => parseInt(b.summand, 10)),
  ];
}

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

assertEquals(input, [
  [13, 12, 10, -11, 14, 13, 12, -5, 10, 0, -11, -13, -13, -11],
  [8, 16, 4, 1, 13, 5, 0, 10, 7, 2, 13, 15, 14, 9],
]);

const [offsets, summands] = input;
const digits = offsets.length;

const results: [number[], number[]] = [new Array(digits), new Array(digits)];

const stack: number[] = [];

for (let i = 0; i < digits; i++) {
  if (offsets[i] > 0) {
    stack.push(i);
  } else {
    const iOld = stack.pop();
    if (iOld === undefined) throw new Error(`Pop on empty stack!? At i=${i}.`);
    const diff = summands[iOld] + offsets[i];
    results[0][i] = 9 + Math.min(0, diff);
    results[0][iOld] = 9 - Math.max(0, diff);
    results[1][i] = 1 + Math.max(0, diff);
    results[1][iOld] = 1 - Math.min(0, diff);
  }
}

const expectedResults = ["96929994293996", "41811761181141"];

for (const [partNumber, result] of results.entries()) {
  const stringResult = result.join("");
  assertEquals(stringResult, expectedResults[partNumber]);
  console.log(`Result part ${partNumber + 1}: ${stringResult}`);
}
