// run with `deno run --allow-read=input.txt main.ts`

const file = await Deno.readTextFile("input.txt");

const regex =
  /Register A: (\d+)\nRegister B: (\d+)\nRegister C: (\d+)\n\nProgram: ((?:\d+,)+\d+)\n/;

const match = file.match(regex);
if (!match) throw new Error("Invalid input");
const [, aString, bString, cString, programString] = match;

let registerA = parseInt(aString, 10);
let registerB = parseInt(bString, 10);
let registerC = parseInt(cString, 10);
const program = programString.split(",").map((n) => parseInt(n, 10));

console.log(programString);

const resolveCombo = (operand: number) => {
  if (operand >= 0 && operand <= 3) return operand;
  if (operand === 4) return registerA;
  if (operand === 5) return registerB;
  if (operand === 6) return registerC;
  throw new Error(`Invalid combo operand: ${operand}`);
};

let ic = 0;

const output = [];

while (ic < program.length && ic >= 0) {
  console.log(ic, registerA, registerB, registerC);
  const opCode = program[ic];
  const operand = program[ic + 1];
  switch (opCode) {
    case 0: { // adv
      console.log("adv", resolveCombo(operand));
      registerA >>= resolveCombo(operand);
      ic += 2;
      break;
    }
    case 1: { // bxl
      console.log("bxl", operand);
      registerB ^= operand;
      ic += 2;
      break;
    }
    case 2: { // bst
      console.log("bst", resolveCombo(operand));
      registerB = resolveCombo(operand) % 8;
      ic += 2;
      break;
    }
    case 3: { // jnz
      console.log("jnz", registerA, operand);
      if (registerA !== 0) {
        ic = operand;
      } else {
        ic += 2;
      }
      break;
    }
    case 4: { // bxc
      console.log("bxc");
      registerB ^= registerC;
      ic += 2;
      break;
    }
    case 5: { // out
      console.log("out", resolveCombo(operand));
      output.push(resolveCombo(operand) % 8);
      ic += 2;
      break;
    }
    case 6: { // bdv
      console.log("bdv", resolveCombo(operand));
      registerB = registerA >> resolveCombo(operand);
      ic += 2;
      break;
    }
    case 7: { // cdv
      console.log("cdv", resolveCombo(operand));
      registerC = registerA >> resolveCombo(operand);
      ic += 2;
      break;
    }
  }
}

console.log(`Part 1: ${output.join(",")}`);
