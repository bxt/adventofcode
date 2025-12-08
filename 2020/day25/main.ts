#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";

const parseInput = (string: string): [number, number] => {
  const numbers = string.trim().split("\n").map(Number);
  assertEquals(numbers.length, 2);
  const [a, b] = numbers;
  return [a, b];
};

const example = parseInput(`
  5764801
  17807724
`);

assertEquals(example, [5764801, 17807724]);

const input = await Deno.readTextFile("input.txt");

const parsedInput = parseInput(input);

const magicNumber = 20201227;

const figureLoopSize = (transformed: number, base = 7): number => {
  let loopSize = 0;
  let result = 1;
  while (true) {
    if (result === transformed) return loopSize;
    loopSize++;
    result = (base * result) % magicNumber;
  }
};

assertEquals(figureLoopSize(5764801), 8);
assertEquals(figureLoopSize(17807724), 11);

const transform = (base: number, loopSize: number): number => {
  let result = 1;
  for (let i = 0; i < loopSize; i++) {
    result = (base * result) % magicNumber;
  }
  return result;
};

const part1 = (doorAndCardPublicKey: [number, number]): number => {
  const [doorPublicKey, cardPublicKey] = doorAndCardPublicKey;
  const doorPrivateKey = figureLoopSize(doorPublicKey);
  const cardPrivateKey = figureLoopSize(cardPublicKey);
  const encryptionKeyFromDoor = transform(doorPublicKey, cardPrivateKey);
  const encryptionKeyFromCard = transform(cardPublicKey, doorPrivateKey);
  assertEquals(encryptionKeyFromDoor, encryptionKeyFromCard);
  return encryptionKeyFromDoor;
};

assertEquals(part1(example), 14897079);

console.log("Result part 1: " + part1(parsedInput));
