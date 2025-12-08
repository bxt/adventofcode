#!/usr/bin/env deno run --allow-read=input.txt

type Position = [number, number];

const blankSpace = ".";

const parse = (input: string): string[] => {
  return input
    .trim()
    .split("\n")
    .map((line) => line.trim());
};

const file = await Deno.readTextFile("input.txt");

const parsedInput = parse(file);

const antennasByFrequency: Record<string, Position[]> = {};

parsedInput.forEach((line, y) => {
  for (let x = 0; x < line.length; x++) {
    if (line[x] !== blankSpace) {
      antennasByFrequency[line[x]] ??= [];
      antennasByFrequency[line[x]].push([x, y]);
    }
  }
});

const isInBounds = ([x, y]: Position) => {
  return (
    x >= 0 && y >= 0 && x < parsedInput[0].length && y < parsedInput.length
  );
};

const subtract = ([x1, y1]: Position, [x2, y2]: Position): Position => {
  return [x1 - x2, y1 - y2];
};

const forEachAntennaPair = (
  callback: (position: Position, delta: Position) => void
) => {
  for (const [_frequency, positions] of Object.entries(antennasByFrequency)) {
    for (const position of positions) {
      for (const other of positions) {
        if (position === other) continue;
        const delta = subtract(other, position);
        callback(position, delta);
      }
    }
  }
};

const antinodePositions = new Set<string>();

forEachAntennaPair((position, delta) => {
  const antinode = subtract(position, delta);
  if (isInBounds(antinode)) {
    antinodePositions.add(antinode.toString());
  }
});

console.log(`Part 1: ${antinodePositions.size}`);

const harmonicAntinodePositions = new Set<string>();

forEachAntennaPair((position, delta) => {
  let antinode = position;
  while (isInBounds(antinode)) {
    harmonicAntinodePositions.add(antinode.toString());
    antinode = subtract(antinode, delta);
  }
});

console.log(`Part 2: ${harmonicAntinodePositions.size}`);
