#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";
import { parse, print } from "https://x.nest.land/swc@0.0.6/mod.ts";
import { Program } from "https://x.nest.land/swc@0.0.6/types/options.ts";
import { ensureElementOf, Maybe, maybeElementOf, sum } from "../utils.ts";

const binaryOperators = ["+", "*"] as const;
type BinaryOperator = typeof binaryOperators[number];
type OperatorPrecedences = Record<BinaryOperator, number>;
type OperatorImplementations = Record<
  BinaryOperator,
  (a: number, b: number) => number
>;
type Operator = BinaryOperator | "(";
type Token = Operator | ")" | number;
type Equation = Token[];

const operatorImplementations: OperatorImplementations = {
  "+": (a, b) => a + b,
  "*": (a, b) => a * b,
};

const operatorPrecedencesPart2: OperatorPrecedences = {
  "+": 2,
  "*": 1,
};

const lexer = (s: string): Equation[] =>
  s.trim().split("\n").map((line) =>
    [...line.matchAll(/(?<literal>\d+)|(?<operator>[+*])|(?<brace>[()])/g)].map(
      (match) => {
        const { groups } = match;
        if (!groups) throw new Error(`Unkown token (1) ${match}`);
        if (groups.literal) return Number(groups.literal);
        if (groups.operator) {
          return ensureElementOf(groups.operator, binaryOperators);
        }
        if (groups.brace === "(") return "(";
        if (groups.brace === ")") return ")";
        throw new Error(`Unkown token (2) ${match}`);
      },
    )
  );

const reduce = (
  resultStack: number[],
  operatorStack: Operator[],
  operatorPrecedence: Maybe<number>,
  operatorPrecedences: OperatorPrecedences,
) => {
  while (true) {
    const operator = operatorStack.pop();
    if (operator === undefined) { // stack empty
      break;
    }
    if (operator === "(") {
      operatorStack.push(operator);
      break;
    }
    if (
      operatorPrecedence !== null &&
      operatorPrecedences[operator] <= operatorPrecedence
    ) {
      operatorStack.push(operator);
      break;
    }

    const b = resultStack.pop();
    const a = resultStack.pop();
    if (typeof a !== "number") {
      throw new Error(`Invalid: ${a}`);
    }
    if (typeof b !== "number") {
      throw new Error(`Invalid: ${b}`);
    }
    console.log({ a, operator, b });
    const result = operatorImplementations[operator](a, b);
    resultStack.push(result);
  }
};

const calculate = (
  equations: Equation[],
  operatorPrecedences: OperatorPrecedences,
): number =>
  sum(equations.map((tokens) => {
    console.log({ tokens });
    const resultStack: number[] = [];
    const operatorStack: Operator[] = [];
    tokens.forEach((token) => {
      console.log({ token, resultStack, operatorStack, start: true });
      switch (token) {
        case "(":
          operatorStack.push("(");
          break;
        case ")": {
          reduce(
            resultStack,
            operatorStack,
            null,
            operatorPrecedences,
          );
          const operator = operatorStack.pop();
          if (operator !== "(") {
            throw new Error(`Invalid: ${operator}`);
          }
          break;
        }
        default: {
          const binaryOperator = maybeElementOf(token, binaryOperators);
          if (binaryOperator !== null) {
            reduce(
              resultStack,
              operatorStack,
              operatorPrecedences[binaryOperator],
              operatorPrecedences,
            );
            operatorStack.push(binaryOperator);
          } else if (typeof token === "number") {
            resultStack.push(token);
          } else {
            throw new Error(`Invalid: ${token}`);
          }
          break;
        }
      }
    });

    reduce(
      resultStack,
      operatorStack,
      null,
      operatorPrecedences,
    );

    if (resultStack.length !== 1) {
      throw new Error(`Invalid: ${resultStack.length}`);
    }
    const result = resultStack[0];
    if (typeof result !== "number") throw new Error(`Invalid: ${result}`);
    console.log({ resultStack, final: true });
    return result;
  }));

const input = await Deno.readTextFile("input.txt");

const example = lexer("1 + 2 * 3 + 4 * 5 + 6");
const exampleWithBraces = lexer("1 + (2 * 3) + (4 * (5 + 6))");
const example2 = lexer("2 * 3 + (4 * 5)");
const complexExample = lexer("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2");

assertEquals(calculate(example, operatorPrecedencesPart2), 231);
assertEquals(calculate(exampleWithBraces, operatorPrecedencesPart2), 51);
assertEquals(calculate(example2, operatorPrecedencesPart2), 46);
assertEquals(
  calculate(complexExample, operatorPrecedencesPart2),
  23340,
);

const part2 = (s: string) => calculate(lexer(input), operatorPrecedencesPart2);

assertEquals(part2(`${example}\n${example2}`), 71 + 26);

console.log("Result part 2: " + part2(input));
