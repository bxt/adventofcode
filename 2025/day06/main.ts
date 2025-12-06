// run with `deno run --allow-read=input.txt main.ts`

const file = await Deno.readTextFile("input.txt");

const lines = file.trim().split("\n");

const problemLines = lines.slice(0, -1);
const operatorsLine = lines[lines.length - 1];

const parseOperator = (char: string): "+" | "*" => {
  if (char === "+" || char === "*") {
    return char;
  } else {
    throw new Error(`Unknown operator: ${char}`);
  }
};

const operatorFunctions = {
  "+": (a: number, b: number) => a + b,
  "*": (a: number, b: number) => a * b,
};

const parsedProblemLines = problemLines.map((line) =>
  line.matchAll(/\d+/g).map(([numberString]) => parseInt(numberString, 10))
    .toArray()
);
const parsedOperatorsLine = operatorsLine.matchAll(/[+*]/g).map((match) =>
  [parseOperator(match[0]), match.index] as const
).toArray();

let part1 = 0;

for (let i = 0; i < parsedOperatorsLine.length; i++) {
  const [operator] = parsedOperatorsLine[i];

  let result = operator === "+" ? 0 : 1;

  for (const problemLine of parsedProblemLines) {
    result = operatorFunctions[operator](result, problemLine[i]);
  }

  part1 += result;
}

console.log(`Part 1: ${part1}`);
