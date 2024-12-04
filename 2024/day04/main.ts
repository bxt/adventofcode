// run with `deno run --allow-read=input.txt main.ts`

const WORD = "XMAS";

const file = await Deno.readTextFile("input.txt");

const lines = file.split("\n").filter((line) => line);

const fourDirections = [
  [1, 0],
  [0, 1],
  [-1, 0],
  [0, -1],
];

const diagonalDirections = [
  [1, 1],
  [-1, 1],
  [1, -1],
  [-1, -1],
];

const eightDirections = [...fourDirections, ...diagonalDirections];

let part1 = 0;

for (let line = 0; line < lines.length; line++) {
  for (let index = 0; index < lines[line].length; index++) {
    withNextDirection: for (const direction of eightDirections) {
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

let part2 = 0;

for (let line = 0; line < lines.length; line++) {
  for (let index = 0; index < lines[line].length; index++) {
    if (lines[line][index] !== "A") continue;
    const [se, ne, sw, nw] = diagonalDirections.map(([dl, di]) => {
      return lines[line + dl]?.[index + di];
    });
    if (!((se === "M" && nw === "S") || (se === "S" && nw === "M"))) continue;
    if (!((ne === "M" && sw === "S") || (ne === "S" && sw === "M"))) continue;
    part2++;
  }
}

console.log(`Part 2: ${part2}`);
