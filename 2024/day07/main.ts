// run with `deno run --allow-read=input.txt main.ts`
import { assertEquals } from "jsr:@std/assert";

type Equation = {
  result: number;
  inputs: number[];
}

const parseEquation = (input: string): Equation => {
  const [resultString, inputsString] = input.trim().split(": ");
  const result = parseInt(resultString, 10);
  const inputs = inputsString.split(" ").map((s) => parseInt(s, 10));
  return { result, inputs };
};


const parse = (input: string): Equation[] => {
  const lines = input.trim().split("\n");
  return lines.map(parseEquation);
};

const operators = ["+", "*"] as const;

const couldFinish = (equation: Equation): boolean => {
  const { result, inputs } = equation;
  let currentResults = [inputs[0]];
  for (let i = 1; i < inputs.length; i++) {
    const newResults = [];
    for (const operator of operators) {
      for (const currentResult of currentResults) {
        if (operator === "+") {
          newResults.push(currentResult + inputs[i]);

        } else if (operator === "*") {
          newResults.push(currentResult * inputs[i]);
        } else {
          const unknownOperator: never = operator;
          throw new Error(`Unknown operator ${unknownOperator}`);
        }
      }
    }
    currentResults = newResults;
  }
  return currentResults.includes(result);
}

{
  const example = `
    190: 10 19
    3267: 81 40 27
    83: 17 5
    156: 15 6
    7290: 6 8 6 15
    161011: 16 10 13
    192: 17 8 14
    21037: 9 7 18 13
    292: 11 6 16 20
  `;
  const parsedInput = parse(example);
  assertEquals(parsedInput[0].result, 190);
  assertEquals(parsedInput[0].inputs, [10, 19]);
  assertEquals(parsedInput.map(couldFinish), [
    true,
    true,
    false,
    false,
    false,
    false,
    false,
    false,
    true,
  ]);
}

const file = await Deno.readTextFile("input.txt");

const parsedInput = parse(file);

const part1 = parsedInput.filter(couldFinish)
  .map((equation) => equation.result)
  .reduce((acc, i) => acc + i, 0);

console.log(`Part 1: ${part1}`);
