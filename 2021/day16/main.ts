#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { ensureElementOf, product, range, sum } from "../../2020/utils.ts";

const binaryDigits = ["0", "1"] as const;
type BinaryDigit = typeof binaryDigits[number];

const parseInput = (string: string): number[] => {
  return string.trim().split("").map((s) => parseInt(s, 16));
};

const SUM_TYPE = 0;
const PRODUCT_TYPE = 1;
const MINIMUM_TYPE = 2;
const MAXIMUM_TYPE = 3;
const LITERAL_VALUE_TYPE = 4;
const GREATER_THAN_TYPE = 5;
const LESS_THAN_TYPE = 6;
const EQUAL_TO_TYPE = 7;

const MODE_BIT_LENGTH = 0;
const MODE_PACKAGE_COUNT = 1;

class Lexer {
  readonly bytes: number[];
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

  hasEnded(): boolean {
    return (this.positon > this.bytes.length * 4);
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
  const type = lexer.get(3);

  switch (type) {
    case LITERAL_VALUE_TYPE: {
      let number = 0;

      const readBit = (): void => {
        lexer.getOne();
        number *= 2 ** 4;
        const numberByte = lexer.get(4);
        number += numberByte;
      };

      while (lexer.peekOne() === 1) readBit();
      readBit();

      return { type: "literal", number, version };
    }
    default: {
      const mode = lexer.getOne();
      const packages: Package[] = [];

      if (mode === MODE_BIT_LENGTH) {
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
      } else if (mode === MODE_PACKAGE_COUNT) {
        const subPackageCount = lexer.get(11);
        range(subPackageCount).forEach((i) => {
          const pkg = parsePakage(lexer);
          if (pkg === undefined) {
            throw new Error(`Expeted package at index ${i} but got none`);
          }
          packages.push(pkg);
        });
      } else {
        throw new Error(`Unexpected mode ${mode}`);
      }

      return { type: "operator", typeId: type, packages, version };
    }
  }
}

const input = await Deno.readTextFile("input.txt");

function sumVersions(pdk: Package): number {
  switch (pdk.type) {
    case "literal":
      return pdk.version;
    default:
      return sum(pdk.packages.map(sumVersions)) + pdk.version;
  }
}

function part1(string: string): number {
  const lexer = new Lexer(string);
  const pkg = parsePakage(lexer);

  while (!lexer.hasEnded()) {
    if (lexer.getOne() !== 0) {
      throw new Error("Expeted only one package");
    }
  }

  return sumVersions(pkg);
}

const example = new Lexer("D2FE28");
assertEquals(example.get(3), 6);
assertEquals(example.get(3), 4);
assertEquals(example.get(5), 23);
assertEquals(example.get(5), 30);
assertEquals(example.get(5), 5);

assertEquals(parsePakage(new Lexer("D2FE28")), {
  type: "literal",
  number: 2021,
  version: 6,
});

assertEquals(parsePakage(new Lexer("38006F45291200")), {
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

assertEquals(parsePakage(new Lexer("EE00D40C823060")), {
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

// 100010100000000001001010100000000001101010000000000000101111010001111000
// VVVTTTILLLLLLLLLLLVVVTTTILLLLLLLLLLLVVVTTTILLLLLLLLLLLLLLLVVVTTTAAAAA
assertEquals(part1("8A004A801A8002F478"), 16);

// 01100010000000001000000000000000000101100001000101010110001011001000100000000010000100011000111000110100
// VVVTTTILLLLLLLLLLLVVVTTTILLLLLLLLLLLLLLLAAAAAAAAAAAAAAAAAAAAAAVVVTTTILLLLLLLLLLLVVVTTTAAAAAVVVTTTAAAAA
assertEquals(part1("620080001611562C8802118E34"), 12);

assertEquals(part1("C0015000016115A2E0802F182340"), 23);

assertEquals(part1("A0016C880162017C3686B18A3D4780"), 31);

console.log("Result part 1: " + part1(input));

function evaluatePackage(pdk: Package): number {
  switch (pdk.type) {
    case "literal":
      return pdk.number;
    default: {
      const packagesEvaluated = pdk.packages.map(evaluatePackage);

      if (packagesEvaluated.length === 0) {
        throw new Error("No operands found");
      }

      const expectTwoOperands = (): void => {
        if (packagesEvaluated.length !== 2) {
          throw new Error("Expected exactly 2 operands");
        }
      };

      switch (pdk.typeId) {
        case SUM_TYPE: {
          return sum(packagesEvaluated);
        }
        case PRODUCT_TYPE: {
          return product(packagesEvaluated);
        }
        case MINIMUM_TYPE: {
          return Math.min(...packagesEvaluated);
        }
        case MAXIMUM_TYPE: {
          return Math.max(...packagesEvaluated);
        }
        case GREATER_THAN_TYPE: {
          expectTwoOperands();
          const [a, b] = packagesEvaluated;
          return a > b ? 1 : 0;
        }
        case LESS_THAN_TYPE: {
          expectTwoOperands();
          const [a, b] = packagesEvaluated;
          return a < b ? 1 : 0;
        }
        case EQUAL_TO_TYPE: {
          expectTwoOperands();
          const [a, b] = packagesEvaluated;
          return a === b ? 1 : 0;
        }
        default:
          throw new Error(`Unknown type id: ${pdk.typeId}`);
      }
    }
  }
}

function part2(string: string): number {
  const lexer = new Lexer(string);
  const pkg = parsePakage(lexer);
  return evaluatePackage(pkg);
}

assertEquals(part2("C200B40A82"), 3);
assertEquals(part2("04005AC33890"), 54);
assertEquals(part2("880086C3E88112"), 7);
assertEquals(part2("CE00C43D881120"), 9);
assertEquals(part2("D8005AC2A8F0"), 1);
assertEquals(part2("F600BC2D8F"), 0);
assertEquals(part2("9C005AC2F8F0"), 0);
assertEquals(part2("9C0141080250320F1802104A08"), 1);

console.log("Result part 2: " + part2(input));
