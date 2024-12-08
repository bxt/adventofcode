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

const isInBounds = ([x, y]: readonly [number, number]) => {
  return (x >= 0 && y >= 0 && x < parsedInput[0].length && y < parsedInput.length);
}

for (const [_frequency, positions] of Object.entries(antennasByFrequency)) {
  for (const [x, y] of positions) {
    for (const [otherX, otherY] of positions) {
      if (x === otherX && y === otherY) continue;
      const deltaX = otherX - x;
      const deltaY = otherY - y;
      const antinode = [x - deltaX, y - deltaY] as const;
      if (isInBounds(antinode)) {
        antinodePositions.add(antinode.toString());
      }
    }
  }
}

console.log(`Part 1: ${antinodePositions.size}`);

const harmonicAntinodePositions = new Set<string>();

for (const [_frequency, positions] of Object.entries(antennasByFrequency)) {
  for (const [x, y] of positions) {
    for (const [otherX, otherY] of positions) {
      if (x === otherX && y === otherY) continue;
      const deltaX = otherX - x;
      const deltaY = otherY - y;
      const antinode: [number, number] = [x, y];
      while (isInBounds(antinode)) {
        harmonicAntinodePositions.add(antinode.toString());
        antinode[0] -= deltaX;
        antinode[1] -= deltaY;
      }
    }
  }
}

console.log(`Part 2: ${harmonicAntinodePositions.size}`);
