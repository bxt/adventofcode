// run with `deno run --allow-read main.ts`

const file = await Deno.readTextFile("input.txt");

const numberPairs = file.split("\n").filter(line => line).map((line) =>
  line.split(/ +/).map((n) => parseInt(n, 10))
);

const sorted = [0, 1].map((i) => numberPairs.map((pair) => pair[i]).sort((a, b) => a - b));

const part1 = numberPairs.map((_, index) => {
  return Math.abs(sorted[0][index] - sorted[1][index]);
}).reduce((accumulator, current) => accumulator + current, 0);

console.log(`Part 1: ${part1}`);