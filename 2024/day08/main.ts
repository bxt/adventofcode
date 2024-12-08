// run with `deno run --allow-read=input.txt main.ts`

const blankSpace = ".";

const parse = (input: string): string[] => {
  return input
    .trim()
    .split("\n")
    .map((line) => line.trim());
};

const file = await Deno.readTextFile("input.txt");

const parsedInput = parse(file);

const antennasByFrequency: Record<string, [number, number][]> = {};

parsedInput.forEach((line, y) => {
  for (let x = 0; x < line.length; x++) {
    if (line[x] !== blankSpace) {
      antennasByFrequency[line[x]] ??= [];
      antennasByFrequency[line[x]].push([x, y]);
    }
  }
});

const antinodePositions = new Set<string>();

const addToAntinodePositionsIfInBounds = (x: number, y: number) => {
  if (x >= 0 && y >= 0 && x < parsedInput[0].length && y < parsedInput.length) {
    antinodePositions.add(`${x},${y}`);
  }
}

for (const [_frequency, positions] of Object.entries(antennasByFrequency)) {
  for (const [x, y] of positions) {
    for (const [otherX, otherY] of positions) {
      if (x === otherX && y === otherY) continue;
      const deltaX = otherX - x;
      const deltaY = otherY - y;
      addToAntinodePositionsIfInBounds(x - deltaX, y - deltaY);
      addToAntinodePositionsIfInBounds(otherX + deltaX, otherY + deltaY);
    }
  }
}

console.log(`Part 1: ${antinodePositions.size}`);
