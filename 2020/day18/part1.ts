#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";
import { parse, print } from "https://x.nest.land/swc@0.0.6/mod.ts";
import { Program } from "https://x.nest.land/swc@0.0.6/types/options.ts";
import { sum } from "../utils.ts";

type Operator = "+" | "*";
type Token = Operator | "(" | ")" | number;
type Equation = Token[];

const lexer = (s: string): Equation[] =>
  s.trim().split("\n").map((line) =>
    [...line.matchAll(/(?<literal>\d+)|(?<operator>[+*])|(?<brace>[()])/g)].map(
      (match) => {
        const { groups } = match;
        if (!groups) throw new Error(`Unkown token (1) ${match}`);
        if (groups.literal) return Number(groups.literal);
        if (groups.operator === "+") return "+";
        if (groups.operator === "*") return "*";
        if (groups.brace === "(") return "(";
        if (groups.brace === ")") return ")";
        throw new Error(`Unkown token (2) ${match}`);
      },
    )
  );

const reduce = (resultStack: (number | Operator | null)[]) => {
  const firstNumber = resultStack.pop();
  const operator = resultStack.pop();
  const otherNumber = resultStack.pop();
  if (typeof firstNumber !== "number") {
    throw new Error(`Invalid: ${firstNumber}`);
  }
  if (typeof otherNumber !== "number") {
    throw new Error(`Invalid: ${otherNumber}`);
  }
  if (operator === "*") {
    console.log({ firstNumber, operator, otherNumber });
    resultStack.push(firstNumber * otherNumber);
  } else if (operator === "+") {
    resultStack.push(firstNumber + otherNumber);
  } else {
    throw new Error("?");
  }
};

const calculate = (equations: Equation[]): number =>
  sum(equations.map((tokens) => {
    console.log({ tokens });
    const resultStack: (number | Operator | null)[] = [0, "+"];
    tokens.forEach((token) => {
      console.log({ resultStack });
      switch (token) {
        case "(":
          resultStack.push(0);
          resultStack.push("+");
          break;
        case ")":
          reduce(resultStack);
          break;
        case "+":
          resultStack.push("+");
          break;

        case "*":
          resultStack.push("*");
          break;

        default: {
          resultStack.push(token);
          console.log({ resultStack, x: 1 });
          reduce(resultStack);
          break;
        }
      }
    });

    if (resultStack.length !== 1) {
      throw new Error(`Invalid: ${resultStack.length}`);
    }
    const result = resultStack[0];
    if (typeof result !== "number") throw new Error(`Invalid: ${result}`);
    console.log({ resultStack });
    return result;
  }));

const input = await Deno.readTextFile("input.txt");

const example = lexer("1 + 2 * 3 + 4 * 5 + 6");
const exampleWithBraces = lexer("1 + (2 * 3) + (4 * (5 + 6))");
const example2 = lexer("2 * 3 + (4 * 5)");

assertEquals(calculate(example), 71);
assertEquals(calculate(exampleWithBraces), 51);
assertEquals(calculate(example2), 26);
assertEquals(
  calculate(lexer("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")),
  13632,
);

assertEquals(calculate(lexer(`${example}\n${example2}`)), 71 + 26);

console.log("Result part 1: " + calculate(lexer(input)));
