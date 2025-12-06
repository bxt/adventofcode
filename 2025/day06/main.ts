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

const parsedOperatorsLine = operatorsLine.matchAll(/[+*]/g).map((match) =>
  [parseOperator(match[0]), match.index] as const
).toArray();

const parsedProblemLines = problemLines.map((line) =>
  line.matchAll(/\d+/g).map(([numberString]) => parseInt(numberString, 10))
    .toArray()
);

const doCephalopodMath = (
  getNumbers: (
    op: { operatorIndex: number; operatorPosition: number },
  ) => number[],
) => {
  let total = 0;

  for (let i = 0; i < parsedOperatorsLine.length; i++) {
    const [operator, operatorPosition] = parsedOperatorsLine[i];

    let result = operator === "+" ? 0 : 1;

    for (const number of getNumbers({ operatorIndex: i, operatorPosition })) {
      result = operatorFunctions[operator](result, number);
    }

    total += result;
  }

  return total;
};

console.log(
  `Part 1: ${
    doCephalopodMath(({ operatorIndex }) =>
      parsedProblemLines.map((line) => line[operatorIndex]!)
    )
  }`,
);

const problemLinesMaxLength = problemLines.reduce(
  (max, line) => Math.max(max, line.length),
  0,
);

console.log(
  `Part 2: ${
    doCephalopodMath(({ operatorIndex, operatorPosition }) => {
      const [_, nextPosition] = parsedOperatorsLine[operatorIndex + 1] ||
        [null, problemLinesMaxLength + 1];
      const numbers: number[] = [];

      for (let k = operatorPosition; k < nextPosition - 1; k++) {
        let nextNumberString = "";
        for (const problemLine of problemLines) {
          nextNumberString += problemLine[k];
        }
        const nextNumber = parseInt(nextNumberString, 10);

        numbers.push(nextNumber);
      }

      return numbers;
    })
  }`,
);
