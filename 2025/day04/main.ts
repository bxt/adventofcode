// run with `deno run --allow-read=input.txt main.ts`

const file = (await Deno.readTextFile("input.txt")).trim();

const lines = file.split("\n");

let part1 = 0;

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

for (let lineIndex = 0; lineIndex < lines.length; lineIndex++) {
  const line = lines[lineIndex];

  for (let charIndex = 0; charIndex < line.length; charIndex++) {
    if (line.charAt(charIndex) !== "@") {
      Deno.stdout.writeSync(new TextEncoder().encode("."));
      continue;
    }

    let occupiedNeighbors = 0;

    for (const [lineOffset, charOffset] of eightNeighbors) {
      const neighborLine = lineIndex + lineOffset;
      const neighborChar = charIndex + charOffset;
      if (lines?.[neighborLine]?.charAt(neighborChar) === "@") {
        occupiedNeighbors++;
      }
    }

    if (occupiedNeighbors < 4) {
      part1++;
      Deno.stdout.writeSync(new TextEncoder().encode("x"));
    } else {
      Deno.stdout.writeSync(new TextEncoder().encode("@"));
    }
  }

  console.log("");
}

console.log(`Part 1: ${part1}`); // 2070 too high
