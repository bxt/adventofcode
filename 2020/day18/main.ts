#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";
import { ensureElementOf, Maybe, maybeElementOf, sum } from "../utils.ts";

const binaryOperators = ["+", "*"] as const;
type BinaryOperator = typeof binaryOperators[number];

type Operator = BinaryOperator | "(";
type Token = Operator | ")" | number;
type Term = Token[];

type OperatorPrecedences = Record<BinaryOperator, number>;
type OperatorImplementations = Record<
  BinaryOperator,
  (a: number, b: number) => number
>;

const operatorImplementations: OperatorImplementations = {
  "+": (a, b) => a + b,
  "*": (a, b) => a * b,
};

const lexer = (s: string): Term[] =>
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

/**
  * Calculates the value of a term, inspired by the Shunting-Yard algorithm
  * but instead of creating Reverse Polish notation (RPN) or an abstract syntax
  * tree (AST) is just caluclates the values directly.
  */
class Evaluator {
  readonly #operatorPrecedences: OperatorPrecedences;
  #resultStack: number[] = [];
  #operatorStack: Operator[] = [];

  constructor(operatorPrecedences: OperatorPrecedences) {
    this.#operatorPrecedences = operatorPrecedences;
  }

  private applyUntilOpeningBraceOrDone(): void {
    this.applyUntilPrecendenceOrOpeningBrace();
  }

  private applyUntilPrecendenceOrOpeningBrace(
    maximumOperatorPrecedence?: number,
  ): void {
    while (this.applyOnce(maximumOperatorPrecedence));
  }

  private applyOnce(maximumOperatorPrecedence?: number): boolean {
    const operator = this.maybeNextOperator(maximumOperatorPrecedence);
    if (operator == null) return false;
    this.applyBinaryOperator(operator);
    return true;
  }

  private maybeNextOperator(
    maximumOperatorPrecedence?: number,
  ): Maybe<BinaryOperator> {
    const operator = this.#operatorStack.pop();

    if (operator === undefined) return null; // stack empty

    if (operator === "(") {
      this.#operatorStack.push(operator);
      return null;
    }

    if (
      maximumOperatorPrecedence !== undefined &&
      this.#operatorPrecedences[operator] < maximumOperatorPrecedence
    ) {
      this.#operatorStack.push(operator);
      return null;
    }

    return operator;
  }

  private applyBinaryOperator(binaryOperator: BinaryOperator): void {
    const b = this.#resultStack.pop();
    const a = this.#resultStack.pop();
    if (typeof a !== "number") {
      throw new Error(`Invalid: ${a}`);
    }
    if (typeof b !== "number") {
      throw new Error(`Invalid: ${b}`);
    }
    const result = operatorImplementations[binaryOperator](a, b);
    this.#resultStack.push(result);
  }

  extractResult(): number {
    if (this.#resultStack.length < 1) {
      throw new Error(
        `Please call calculateResult() frist: ${this.#resultStack.length}`,
      );
    }
    if (this.#resultStack.length > 1) {
      throw new Error(
        `Too many opening braces: ${this.#resultStack.length}`,
      );
    }
    const result = this.#resultStack[0];
    if (typeof result !== "number") {
      throw new Error(`Weird order of tokens: ${result}`);
    }
    return result;
  }

  calculateResult(equation: Term): number {
    this.#resultStack = [];
    this.#operatorStack = [];

    equation.forEach((token) => {
      switch (token) {
        case "(":
          this.#operatorStack.push("(");
          break;
        case ")": {
          this.applyUntilOpeningBraceOrDone();
          const operator = this.#operatorStack.pop();
          if (operator !== "(") {
            throw new Error(`Too many closing braces: ${operator}`);
          }
          break;
        }
        default: {
          const binaryOperator = maybeElementOf(token, binaryOperators);
          if (binaryOperator !== null) {
            this.applyUntilPrecendenceOrOpeningBrace(
              this.#operatorPrecedences[binaryOperator],
            );
            this.#operatorStack.push(binaryOperator);
          } else if (typeof token === "number") {
            this.#resultStack.push(token);
          } else {
            throw new Error(`Invalid: ${token}`);
          }
          break;
        }
      }
    });

    this.applyUntilOpeningBraceOrDone();

    return this.extractResult();
  }
}

const evaluateAll = (
  equations: Term[],
  evaluator: Evaluator,
): number => sum(equations.map((tokens) => evaluator.calculateResult(tokens)));

const input = await Deno.readTextFile("input.txt");

const example = lexer("1 + 2 * 3 + 4 * 5 + 6");
const exampleWithBraces = lexer("1 + (2 * 3) + (4 * (5 + 6))");
const example2 = lexer("2 * 3 + (4 * 5)");
const complexExample = lexer("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2");

const evaluatorPart1 = new Evaluator({
  "+": 1,
  "*": 1,
});

assertEquals(evaluateAll(example, evaluatorPart1), 71);
assertEquals(evaluateAll(exampleWithBraces, evaluatorPart1), 51);
assertEquals(evaluateAll(example2, evaluatorPart1), 26);
assertEquals(
  evaluateAll(complexExample, evaluatorPart1),
  13632,
);

assertEquals(
  evaluateAll(lexer(`${example}\n${example2}`), evaluatorPart1),
  71 + 26,
);

const part1 = evaluateAll(lexer(input), evaluatorPart1);

console.log("Result part 1: " + part1);

const evaluatorPart2 = new Evaluator({
  "+": 2,
  "*": 1,
});

assertEquals(evaluateAll(example, evaluatorPart2), 231);
assertEquals(evaluateAll(exampleWithBraces, evaluatorPart2), 51);
assertEquals(evaluateAll(example2, evaluatorPart2), 46);
assertEquals(
  evaluateAll(complexExample, evaluatorPart2),
  23340,
);

assertEquals(
  evaluateAll(lexer(`${example}\n${example2}`), evaluatorPart2),
  231 + 46,
);

const part2 = evaluateAll(lexer(input), evaluatorPart2);

console.log("Result part 2: " + part2);
