#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";

const inputs = [9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 8, 9, 9];

const offsets = [13, 12, 10, -11, 14, 13, 12, -5, 10, 0, -11, -13, -13, -11];

const summands = [8, 16, 4, 1, 13, 5, 0, 10, 7, 2, 13, 15, 14, 9];

let z = 0;

for (let i = 0; i < 14; i++) {
  const input = inputs[i];

  const equals = z % 26 + offsets[i] === input;

  const divZ = offsets[i] <= 0 ? 26 : 1;
  z = Math.floor(z / divZ);

  if (equals) {
    z *= 26;
    z += input + summands[i];
  }

  console.log({ input, equals, z });
}

assertEquals(z, 0);
