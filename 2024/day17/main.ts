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

  while (ic < program.length && ic >= 0 && output.length < program.length) {
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
        registerB = resolveCombo(operand) & BigInt(7);
        ic += 2;
        break;
      }
      case 3: {
        // jnz
        if (registerA !== BigInt(0)) {
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
        output.push(resolveCombo(operand) & BigInt(7));
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

const needle = program;

for (let a = BigInt(136904920099220); true; a++) {
  const output = runWithA(a);
  if (output.length === needle.length) {
    if (output.every((n, i) => n === BigInt(needle[i]))) {
      console.log(`Part 2: ${a}`);
      break;
    }
  }

  if (a % BigInt(10000000) === BigInt(0)) {
    console.log(a);
  }
}