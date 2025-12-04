// run with `deno run --allow-read=input.txt main.ts`

const file = (await Deno.readTextFile("input.txt")).trim();

const lines = file.split("\n");

const eightNeighbors = [
  [-1, -1],
  [-1, 0],
  [-1, 1],
  [0, -1],
  [0, 1],
  [1, -1],
  [1, 0],
  [1, 1],
];

const field = lines.map((line) =>
  Array.from(line).map((char) => (char === "@" ? true : false))
);

let part1: undefined | number = undefined;
let part2: number = 0;

while (true) {
  const toRemove = [];

  for (let lineIndex = 0; lineIndex < field.length; lineIndex++) {
    const line = field[lineIndex];

    for (let charIndex = 0; charIndex < line.length; charIndex++) {
      if (!line[charIndex]) continue;

      const occupiedNeighbors =
        eightNeighbors.filter(([lineOffset, charOffset]) =>
          field?.[lineIndex + lineOffset]?.[charIndex + charOffset]
        ).length;

      if (occupiedNeighbors < 4) {
        toRemove.push([lineIndex, charIndex]);
      }
    }
  }

  for (const [lineIndex, charIndex] of toRemove) {
    field[lineIndex][charIndex] = false;
  }

  if (part1 === undefined) part1 = toRemove.length;
  part2 += toRemove.length;
  if (toRemove.length === 0) break;
}

console.log(`Part 1: ${part1}`);
console.log(`Part 2: ${part2}`);
