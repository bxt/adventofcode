#!/usr/bin/env deno run --allow-read=input.txt

export type Coord3 = readonly [number, number, number];

const file = await Deno.readTextFile("input.txt");

const lines = file.trim().split("\n");

export const positions = lines.map((line) => {
  const [x, y, z] = line.split(",").map((part) => parseInt(part, 10));
  return [x, y, z] as const;
});

export const connections: { from: number; to: number; distance: number }[] = [];

const calculateDistanceSquared = ([x1, y1, z1]: Coord3, [x2, y2, z2]: Coord3) =>
  (x2 - x1) ** 2 + (y2 - y1) ** 2 + (z2 - z1) ** 2;

for (let from = 0; from < positions.length; from++) {
  for (let to = from + 1; to < positions.length; to++) {
    const distance = calculateDistanceSquared(positions[from], positions[to]);
    connections.push({ from, to, distance });
  }
}

connections.sort((a, b) => a.distance - b.distance);

export const getFrequencyCounts = (numbers: number[]): number[] =>
  Object.values(Object.groupBy(numbers, (id) => id)).map(
    (members) => members?.length || 0,
  ).sort((a, b) => b - a);

if (import.meta.main) {
  const componentIds = positions.map((_, index) => index);

  let componentCount = componentIds.length;

  for (
    let connectionId = 0;
    connectionId < connections.length;
    connectionId++
  ) {
    const { from, to } = connections[connectionId];
    const fromComponentId = componentIds[from];
    const toComponentId = componentIds[to];

    if (fromComponentId !== toComponentId) {
      componentCount--;
      for (let i = 0; i < componentIds.length; i++) {
        if (componentIds[i] === toComponentId) {
          componentIds[i] = fromComponentId;
        }
      }
    }

    if (connectionId === 999) {
      const part1 = getFrequencyCounts(componentIds).slice(0, 3)
        .reduce((a, b) => a * b, 1);

      console.log(`Part 1: ${part1}`);
    }

    if (componentCount === 1) {
      console.log(`Part 2: ${positions[from][0] * positions[to][0]}`);
      break;
    }
  }
}
