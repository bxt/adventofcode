#!/usr/bin/env deno run --allow-read=input.txt

const range = (from: number, to: number): number[] =>
  Array.from({ length: to - from }, (_, i) => i + from);

const file = await Deno.readTextFile("input.txt");

const lines = file.trim().split("\n");

const problemLines = lines.slice(0, -1);
const operatorsLine = lines[lines.length - 1];

const parseOperator = (char: string): "+" | "*" => {
  if (char === "+" || char === "*") return char;
  else throw new Error(`Unknown operator: ${char}`);
};

const sum = (numbers: number[]): number => numbers.reduce((a, b) => a + b, 0);
const prod = (numbers: number[]): number => numbers.reduce((a, b) => a * b, 1);
const operatorFunctions = { "+": sum, "*": prod };

const parsedOperatorsLine = operatorsLine.matchAll(/[+*]/g).map((match) => ({
  operator: parseOperator(match[0]),
  operatorPosition: match.index,
})).toArray();

type OperatorInfo = { operatorIndex: number; operatorPosition: number };
type NumbersExtractor = (operatorInfo: OperatorInfo) => number[];

const doCephalopodMath = (getNumbers: NumbersExtractor) => {
  const results = parsedOperatorsLine.map((operatorData, operatorIndex) => {
    const { operator, operatorPosition } = operatorData;
    const numbers = getNumbers({ operatorIndex, operatorPosition });
    return operatorFunctions[operator](numbers);
  });
  return sum(results);
};

const parsedProblemLines = problemLines.map((line) =>
  line.matchAll(/\d+/g).map(([numberString]) => parseInt(numberString, 10))
    .toArray()
);

const extractNumbersPart1: NumbersExtractor = ({ operatorIndex }) =>
  parsedProblemLines.map((line) => line[operatorIndex]!);

console.log(`Part 1: ${doCephalopodMath(extractNumbersPart1)}`);

const problemLinesMaxLength = Math.max(...problemLines.map((l) => l.length));
const sentinelOperatorInfo = { operatorPosition: problemLinesMaxLength + 1 };

const extractNumbersPart2: NumbersExtractor = (operatorInfo) => {
  const { operatorIndex, operatorPosition } = operatorInfo;
  const { operatorPosition: nextPosition } =
    parsedOperatorsLine[operatorIndex + 1] || sentinelOperatorInfo;

  return range(operatorPosition, nextPosition - 1).map((k) =>
    parseInt(problemLines.map((line) => line[k]).join(""), 10)
  );
};

console.log(`Part 2: ${doCephalopodMath(extractNumbersPart2)}`);
