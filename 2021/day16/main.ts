#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { minBy } from "https://deno.land/std@0.116.0/collections/mod.ts";
import {
  addCoords,
  Coord,
  ensureElementOf,
  range,
  sum,
} from "../../2020/utils.ts";

const binaryDigits = ["0", "1"] as const;
type BinaryDigit = typeof binaryDigits[number];

const parseInput = (string: string): number[] => {
  return string.trim().split("").map((s) => parseInt(s, 16));
};

const LITERAL_VALUE_TYPE = 4;

class Lexer {
  bytes: number[];
  positon = 0;

  constructor(bytesString: string) {
    this.bytes = parseInput(bytesString);
  }

  get(amount: number): number {
    return parseInt(this.getBits(amount).join(""), 2);
  }

  getBits(amount: number): BinaryDigit[] {
    return range(amount).map(() =>
      ensureElementOf(this.getOne().toString(), binaryDigits)
    );
  }

  getOne(): number {
    const value = this.peekOne();
    this.positon++;
    return value;
  }

  peekOne(): number {
    const byteIndex = Math.floor(this.positon / 4);
    const bitIndex = this.positon % 4;
    if (byteIndex > this.bytes.length) return 0;
    const byte = this.bytes[byteIndex];
    return (byte >> 4 - (bitIndex + 1)) & 1;
  }
}

type Package =
  & { version: number }
  & (
    | { type: "literal"; number: number }
    | { type: "operator"; typeId: number; packages: Package[] }
  );

function parsePakage(lexer: Lexer): Package {
  const version = lexer.get(3);

  console.log({ version });

  const type = lexer.get(3);

  switch (type) {
    case LITERAL_VALUE_TYPE: {
      let number = 0;
      while (lexer.peekOne() === 1) {
        lexer.getOne();
        number <<= 4;
        const numberByte = lexer.get(4);
        console.log({ numberByte });
        number += numberByte;
      }
      {
        lexer.getOne();
        number <<= 4;
        const numberByte = lexer.get(4);
        console.log({ numberByte });
        number += numberByte;
      }
      console.log({ number });
      return { type: "literal", number, version };
    }
    default: {
      const mode = lexer.getOne();
      const packages: Package[] = [];

      if (mode === 0) {
        const totalLength = lexer.get(15);
        const startPosition = lexer.positon;
        while (lexer.positon < startPosition + totalLength) {
          const pkg = parsePakage(lexer);
          if (pkg === undefined) {
            throw new Error(
              `Expeted package at positon ${lexer.positon} but got none`,
            );
          }
          packages.push(pkg);
        }
      } else if (mode === 1) {
        const subPackageCount = lexer.get(11);
        console.log({ subPackageCount });
        range(subPackageCount).forEach((i) => {
          const pkg = parsePakage(lexer);
          if (pkg === undefined) {
            throw new Error(`Expeted package at index ${i} but got none`);
          }
          packages.push(pkg);
        });
      } else {
        throw new Error();
      }

      return { type: "operator", typeId: type, packages, version };
    }
  }
}

const input = await Deno.readTextFile("input.txt");

const sumVersions = (pdk: Package): number => {
  switch (pdk.type) {
    case "literal":
      return pdk.version;
    default:
      return sum(pdk.packages.map(sumVersions)) + pdk.version;
  }
};

const part1 = (string: string): number => {
  const lexer = new Lexer(string);
  const pkg = parsePakage(lexer);
  if (pkg === undefined) throw new Error("Did not find a package");
  return sumVersions(pkg);
};

const example1 = new Lexer(`D2FE28`);
assertEquals(example1.get(3), 6);
assertEquals(example1.get(3), 4);
assertEquals(example1.get(5), 23);
assertEquals(example1.get(5), 30);
assertEquals(example1.get(5), 5);

const example2 = new Lexer(`D2FE28`);
assertEquals(parsePakage(example2), {
  type: "literal",
  number: 2021,
  version: 6,
});

const example3 = new Lexer(`38006F45291200`);
assertEquals(parsePakage(example3), {
  type: "operator",
  typeId: 6,
  version: 1,
  packages: [{
    type: "literal",
    number: 10,
    version: 6,
  }, {
    type: "literal",
    number: 20,
    version: 2,
  }],
});

const example4 = new Lexer(`EE00D40C823060`);
assertEquals(parsePakage(example4), {
  type: "operator",
  typeId: 3,
  version: 7,
  packages: [{
    type: "literal",
    number: 1,
    version: 2,
  }, {
    type: "literal",
    number: 2,
    version: 4,
  }, {
    type: "literal",
    number: 3,
    version: 1,
  }],
});

console.log("####1");
// 100010100000000001001010100000000001101010000000000000101111010001111000
// VVVTTTILLLLLLLLLLLVVVTTTILLLLLLLLLLLVVVTTTILLLLLLLLLLLLLLLVVVTTTAAAAA
assertEquals(part1("8A004A801A8002F478"), 16);

console.log("####2");
// 01100010000000001000000000000000000101100001000101010110001011001000100000000010000100011000111000110100;
// VVVTTTILLLLLLLLLLLVVVTTTILLLLLLLLLLLLLLLAAAAAAAAAAAAAAAAAAAAAAVVVTTTILLLLLLLLLLLVVVTTTAAAAAVVVTTTAAAAA
assertEquals(part1("620080001611562C8802118E34"), 12);

console.log("####3");
assertEquals(part1("C0015000016115A2E0802F182340"), 23);

console.log("####4");
assertEquals(part1("A0016C880162017C3686B18A3D4780"), 31);

console.log("#### P1");
console.log("Result part 1: " + part1(input));
