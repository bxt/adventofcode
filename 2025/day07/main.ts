// run with `deno run --allow-read=input.txt main.ts`

const file = await Deno.readTextFile("input.txt");

const lines = file.trim().split("\n");

const [startLine, ...manifoldLines] = lines;

const startPosition = startLine.indexOf("S");

let splitCount = 0;
let beamPositions: Set<number> = new Set([startPosition]);

for (const line of manifoldLines) {
  const newBeamPositions: Set<number> = new Set();

  for (const position of beamPositions) {
    if (line[position] === "^") {
      newBeamPositions.add(position - 1);
      newBeamPositions.add(position + 1);
      splitCount++;
    } else {
      newBeamPositions.add(position);
    }
  }

  beamPositions = newBeamPositions;
}

console.log(`Part 1: ${splitCount}`);
