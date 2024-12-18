// run with `deno run --allow-read=input.txt main.ts`

const file = await Deno.readTextFile("input.txt");

const regex =
  /Register A: (\d+)\nRegister B: (\d+)\nRegister C: (\d+)\n\nProgram: ((?:\d+,)+\d+)\n/;

const match = file.match(regex);
if (!match) throw new Error("Invalid input");
const [, aString, bString, cString, programString] = match;

const program = programString.split(",").map((n) => parseInt(n, 10));

const originalRegisterA = BigInt(aString);
const originalRegisterB = BigInt(bString);
const originalRegisterC = BigInt(cString);

const runWithA = (a: bigint): bigint[] => {
  let ic = 0;

  const resolveCombo = (operand: number): bigint => {
    if (operand >= 0 && operand <= 3) return BigInt(operand);
    if (operand === 4) return registerA;
    if (operand === 5) return registerB;
    if (operand === 6) return registerC;
    throw new Error(`Invalid combo operand: ${operand}`);
  };

  let registerA = a;
  let registerB = originalRegisterB;
  let registerC = originalRegisterC;

  const output = [];

  while (ic < program.length && ic >= 0) {
    const opCode = program[ic];
    const operand = program[ic + 1];
    switch (opCode) {
      case 0: {
        // adv
        registerA >>= resolveCombo(operand);
        ic += 2;
        break;
      }
      case 1: {
        // bxl
        registerB ^= BigInt(operand);
        ic += 2;
        break;
      }
      case 2: {
        // bst
        registerB = resolveCombo(operand) & 7n;
        ic += 2;
        break;
      }
      case 3: {
        // jnz
        if (registerA !== 0n) {
          ic = operand;
        } else {
          ic += 2;
        }
        break;
      }
      case 4: {
        // bxc
        registerB ^= registerC;
        ic += 2;
        break;
      }
      case 5: {
        // out
        output.push(resolveCombo(operand) & 7n);
        ic += 2;
        break;
      }
      case 6: {
        // bdv
        registerB = registerA >> resolveCombo(operand);
        ic += 2;
        break;
      }
      case 7: {
        // cdv
        registerC = registerA >> resolveCombo(operand);
        ic += 2;
        break;
      }
    }
  }

  return output;
};

console.log(`Part 1: ${runWithA(originalRegisterA).join(",")}`);

const loopTemplate: (number | "*")[] = [1, "*", 7, 5, 1, "*", 0, 3, 4, 6];
const template: (number | "*")[] = [2, 4, ...loopTemplate, 5, 5, 3, 0];

if (template.length !== program.length) {
  throw new Error("Program did not match template by length");
}

const [positionShift, secondShift] = template
  .map((n, i) => {
    const programValue = program[i];
    if (n === "*") return BigInt(programValue);
    if (n !== program[i]) {
      throw new Error(`Program does not match template at ${i}`);
    }
    return undefined;
  })
  .filter((n) => n !== undefined);

const resultShift = positionShift ^ secondShift;

/**
 * Program from `loopTemplate` re-written in TypeScript.
 */
const programLoopContents = (a: bigint, b: bigint): bigint => {
  const bShift = b ^ positionShift;
  const c = a >> bShift;
  const bResult = (b ^ resultShift ^ c) & 0b111n;
  return bResult;
};

const searchAnswer = (
  needle: number[],
  needleIndex: number,
  input: bigint
): bigint | undefined => {
  input <<= 3n;
  const current = BigInt(needle[needleIndex]);

  for (let b = 0n; b < 8n; b++) {
    const newInput = input | b;
    if (programLoopContents(newInput, b) === current) {
      if (needleIndex === 0) return newInput;
      const result = searchAnswer(needle, needleIndex - 1, newInput);
      if (result !== undefined) return result;
    }
  }

  return undefined;
};

const reproducesProgram = searchAnswer(program, program.length - 1, 0n);
if (reproducesProgram === undefined) throw new Error("No answer found.");

const output = runWithA(reproducesProgram);

if (output.length !== program.length) {
  throw new Error("Program did not reproduce by length");
}
if (!output.every((n, i) => n === BigInt(program[i]))) {
  throw new Error("Program did not reproduce by value");
}

console.log(`Part 2: ${reproducesProgram}`);
