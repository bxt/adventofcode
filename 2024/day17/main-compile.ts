// run with `deno run --allow-read=input.txt main.ts`

const file = await Deno.readTextFile("input.txt");

const regex =
  /Register A: (\d+)\nRegister B: (\d+)\nRegister C: (\d+)\n\nProgram: ((?:\d+,)+\d+)\n/;

const match = file.match(regex);
if (!match) throw new Error("Invalid input");
const [, aString, bString, cString, programString] = match;

const program = programString.split(",").map((n) => parseInt(n, 10));

const originalRegisterA = parseInt(aString, 10);
const originalRegisterB = parseInt(bString, 10);
const originalRegisterC = parseInt(cString, 10);

const compileCombo = (operand: number) => {
  if (operand >= 0 && operand <= 3) return operand;
  if (operand === 4) return "a";
  if (operand === 5) return "b";
  if (operand === 6) return "c";
  throw new Error(`Invalid combo operand: ${operand}`);
};

const compile = (program: number[]): string => {
  let output = `(input, needle = [${program}]) => {\n`;
  output += `  let needleIndex = 0;\n`;
  output += "  let a = input;\n";
  output += `  let b = ${originalRegisterB};\n`;
  output += `  let c = ${originalRegisterC};\n`;
  output += "\n";

  output += "  while (a !== 0) {\n";

  let jnzCount = 0;

  for (let i = 0; i < program.length; i += 2) {
    const opCode = program[i];
    const operand = program[i + 1];
    switch (opCode) {
      case 0: {
        // adv
        output += `    a >>= ${compileCombo(operand)};\n`;
        break;
      }
      case 1: {
        // bxl
        output += `    b ^= ${operand};\n`;
        break;
      }
      case 2: {
        // bst
        output += `    b = ${compileCombo(operand)} % 8;\n`;
        break;
      }
      case 3: {
        // jnz
        if (operand !== 0)  throw new Error("can only jump to start :D");
        if (jnzCount !== 0) throw new Error("can only jump once");
        output += `    // jnz\n`;
        jnzCount++;
        break;
      }
      case 4: {
        // bxc
        output += `    b ^= c;\n`;
        break;
      }
      case 5: {
        // out
        output += `    if (needle[needleIndex++] !== ${compileCombo(operand)} % 8) return false;\n`;
        output += `    if (needleIndex > needle.length) return false;\n`;
        break;
      }
      case 6: {
        // bdv
        output += `    b = a >> ${compileCombo(operand)};\n`;
        break;
      }
      case 7: {
        // cdv
        output += `    c = a >> ${compileCombo(operand)};\n`;
        break;
      }
    }
  }

  if (jnzCount !== 1) throw new Error("should jump once");

  output += "  }\n";
  output += "\n";
  output += "  return needleIndex === needle.length;\n";
  output += "}\n";

  return output;
}

const code = compile(program);

console.log('---');
console.log(code);
console.log('---');

const fn = eval(code);

const part1Needle = [3,6,3,7,0,7,0,3,0];

if (!fn(originalRegisterA, part1Needle)) {
  throw new Error("Part 1 failed");
}

console.log(`Part 1: ${part1Needle.join(",")}`);

for (let a = 1; true; a++) {
  const output = fn(a);
  if (output) {
    console.log(`Part 2: ${a}`);
    break;
  }

  if (a % 10000000 === 0) {
    console.log(a);
  }
}