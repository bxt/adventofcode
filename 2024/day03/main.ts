// run with `deno run --allow-read=input.txt main.ts`

const file = await Deno.readTextFile("input.txt");

const regex = /mul\((\d{1,3}),(\d{1,3})\)|(do)(n't)?\(\)/g;

const instructions = Array.from(
  file.matchAll(regex).map((match) => {
    const [_, a, b, theDo, nt] = match;
    if (theDo) {
      return { kind: "do" as const, enable: !nt };
    } else {
      return { kind: "mul" as const, value: parseInt(a, 10) * parseInt(b, 10) };
    }
  })
);

const part1 = instructions
  .filter((i) => i.kind === "mul")
  .reduce((acc, i) => acc + i.value, 0);

console.log(`Part 1: ${part1}`);

let part2 = 0;
let enabled = true;

for (const i of instructions) {
  if (i.kind === "mul") {
    if (enabled) part2 += i.value;
  } else {
    enabled = i.enable;
  }
}

console.log(`Part 2: ${part2}`);
