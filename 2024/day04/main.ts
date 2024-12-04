// run with `deno run --allow-read=input.txt main.ts`

const WORD = "XMAS";

const file = await Deno.readTextFile("input.txt");

const lines = file.split("\n").filter((line) => line);

const directions = [
  [1, 0],
  [0, 1],
  [-1, 0],
  [0, -1],
  [1, 1],
  [-1, 1],
  [1, -1],
  [-1, -1],
];

let part1 = 0;

for (let line = 0; line < lines.length; line++) {
  for (let index = 0; index < lines[line].length; index++) {
    withNextDirection: for (const direction of directions) {
      for (let wordIndex = 0; wordIndex < WORD.length; wordIndex++) {
        const searchLine = line + direction[0] * wordIndex;
        const searchIndex = index + direction[1] * wordIndex;
        if (lines[searchLine]?.[searchIndex] !== WORD[wordIndex]) {
          continue withNextDirection;
        }
      }
      part1++;
    }
  }
}

console.log(`Part 1: ${part1}`);
