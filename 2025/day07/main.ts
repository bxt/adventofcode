// run with `deno run --allow-read=input.txt main.ts`

const file = await Deno.readTextFile("input.txt");

const lines = file.trim().split("\n");

const [startLine, ...manifoldLines] = lines;

const startPosition = startLine.indexOf("S");

let splitCount = 0;
let beamPositions: Record<number, number> = { [startPosition]: 1 };

for (const line of manifoldLines) {
  const newBeamPositions: Record<number, number> = {};

  for (const [positionString, count] of Object.entries(beamPositions)) {
    const position = Number(positionString);
    if (line[position] === "^") {
      newBeamPositions[position - 1] ||= 0;
      newBeamPositions[position - 1] += count;
      newBeamPositions[position + 1] ||= 0;
      newBeamPositions[position + 1] += count;
      splitCount++;
    } else {
      newBeamPositions[position] ||= 0;
      newBeamPositions[position] += count;
    }
  }

  beamPositions = newBeamPositions;
}

const beamCount = Object.values(beamPositions).reduce((a, b) => a + b, 0);

console.log(`Part 1: ${splitCount}`);
console.log(`Part 2: ${beamCount}`);
