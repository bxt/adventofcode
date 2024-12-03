// run with `deno run --allow-read=input.txt main.ts`

const file = await Deno.readTextFile("input.txt");

const regex = /mul\((\d{1,3}),(\d{1,3})\)/g;

const part1 = file.matchAll(regex).map((match) => {
  const [_, a, b] = match;
  return parseInt(a, 10) * parseInt(b, 10);
}).reduce((acc, curr) => acc + curr, 0);

console.log(`Part 1: ${part1}`);
