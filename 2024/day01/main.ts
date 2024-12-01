// run with `deno run --allow-read=input.txt main.ts`

const file = await Deno.readTextFile("input.txt");

const numberPairs = file
  .split("\n")
  .filter((line) => line)
  .map((line) => line.split(/ +/).map((n) => parseInt(n, 10)));

const [left, right] = [0, 1].map((i) =>
  numberPairs.map((pair) => pair[i]).sort((a, b) => a - b)
);

const part1 = numberPairs
  .map((_, index) => Math.abs(left[index] - right[index]))
  .reduce((accumulator, current) => accumulator + current, 0);

console.log(`Part 1: ${part1}`);

let part2 = 0;
let rightIndex = 0;

for (const leftNumber of left) {
  while (right[rightIndex] < leftNumber) {
    rightIndex++;
  }
  while (right[rightIndex] === leftNumber) {
    part2 += leftNumber;
    rightIndex++;
  }
}

console.log(`Part 2: ${part2}`);
