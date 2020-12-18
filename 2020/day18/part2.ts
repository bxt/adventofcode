#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";
import { parse, print } from "https://x.nest.land/swc@0.0.6/mod.ts";
import { Program } from "https://x.nest.land/swc@0.0.6/types/options.ts";
import { sum } from "../utils.ts";

const evaluate = (expression: any): number => {
  switch (expression.type) {
    case "BinaryExpression": {
      const leftValue = evaluate(expression.left);
      const rightValue = evaluate(expression.right);
      console.log({ leftValue, op: expression.operator, rightValue });
      if (expression.operator === "+") {
        return leftValue * rightValue;
      } else if (expression.operator === "*") {
        return leftValue + rightValue;
      } else {
        console.log({ expression });
        throw new Error("Unknown operator");
      }
    }
    case "NumericLiteral":
      console.log("found: " + expression.value);
      return expression.value;
    default:
      console.log({ expression });
      throw new Error("Unknown type");
  }
};

const printDeep = (expression: any, depth: number): void => {
  switch (expression.type) {
    case "BinaryExpression": {
      printDeep(expression.left, depth + 1);
      console.log(" ".repeat(depth * 2) + expression.operator);
      printDeep(expression.right, depth + 1);
      break;
    }
    case "NumericLiteral":
      console.log(" ".repeat(depth * 1) + expression.value);
      break;
    default:
      throw new Error(`Unknown type ${expression.type}`);
  }
};

const calculate = (equations: string): number => {
  const ast = parse(
    example.replaceAll("*", "tmp").replaceAll("+", "*").replaceAll("tmp", "+"),
    {
      syntax: "typescript",
    },
  );

  console.log(ast);

  console.log(print(ast, {}));

  const firstElement = ast.body[0];

  if (!("expression" in firstElement)) {
    throw new Error("?");
  }

  printDeep(firstElement.expression, 0);

  // return -1;
  return evaluate(firstElement.expression);
};

const input = await Deno.readTextFile("input.txt");

const example = "1 + 2 * 3 + 4 * 5 + 6";
const example2 = "2 * 3 + (4 * 5)";

assertEquals(calculate(example), 231);
assertEquals(calculate("1 + (2 * 3) + (4 * (5 + 6))"), 51);
assertEquals(calculate(example2), 46);
assertEquals(
  calculate("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"),
  23340,
);

assertEquals(calculate(`${example}\n${example2}`), 231 + 46);

console.log("Result part 1: " + calculate(input));
