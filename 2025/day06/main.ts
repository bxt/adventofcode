// run with `deno run --allow-read=input.txt main.ts`

const file = await Deno.readTextFile("input.txt");

const lines = file.trim().split("\n");

const problemLines = lines.slice(0, -1);
const operatorsLine = lines[lines.length - 1];

const parsedProblemLines = problemLines.map((line) =>
  line.matchAll(/\d+/g).map(([numberString]) => parseInt(numberString, 10))
    .toArray()
);
const parsedOperatorsLine = operatorsLine.matchAll(/[+*]/g).map(([operator]) =>
  operator
).toArray();

console.log(parsedProblemLines.map((l) => l.length));
console.log(parsedOperatorsLine.length);

let part1 = 0;

for (let i = 0; i < parsedOperatorsLine.length; i++) {
  const operator = parsedOperatorsLine[i];

  let result = operator === "+" ? 0 : 1;

  for (const problemLine of parsedProblemLines) {
    const nextNumber = problemLine[i];

    if (operator === "+") {
      result += nextNumber;
    } else if (operator === "*") {
      result *= nextNumber;
    } else {
      throw new Error(`Unknown operator: ${operator}`);
    }
  }

  part1 += result;
}

console.log(`Part 1: ${part1}`);
