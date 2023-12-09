#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";

const offsets = [13, 12, 10, -11, 14, 13, 12, -5, 10, 0, -11, -13, -13, -11];

const summands = [8, 16, 4, 1, 13, 5, 0, 10, 7, 2, 13, 15, 14, 9];

const stack = [];
const inputsP1 = new Array(14);

for (let i = 0; i < 14; i++) {
  if (offsets[i] > 0) {
    stack.push(i);
  } else {
    const iOld = stack.pop();
    if (iOld === undefined) throw new Error(`Pop on empty stack!? At i=${i}.`);
    const diff = summands[iOld] + offsets[i];
    if (diff < 0) {
      inputsP1[i] = 9 + diff;
      inputsP1[iOld] = 9;
    } else {
      inputsP1[i] = 9;
      inputsP1[iOld] = 9 - diff;
    }
  }
}

const inputsDebug = [1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5];

const inputs = inputsP1;

let z = 0;

for (let i = 0; i < 14; i++) {
  const input = inputs[i];

  const checkW = z % 26 + offsets[i] !== input;
  // ^^^ can also only ever be true if (offsets[i] <= 0)

  const divZ = offsets[i] <= 0 ? 26 : 1;
  z = Math.floor(z / divZ);

  const factor = (checkW ? 1 : 26);
  z *= factor;

  if (checkW) {
    const summand = input + summands[i];

    z += summand;
  }

  console.log({ input, checkW, factor, z });
}

assertEquals(z, 0);

console.log(inputs.join(""));
