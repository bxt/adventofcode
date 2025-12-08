// run with `deno run --allow-read=input.txt main.ts`

const file = await Deno.readTextFile("input.txt");

const lines = file.trim().split("\n");

const positions = lines.map((line) => {
  const [x, y, z] = line.split(",").map((part) => parseInt(part, 10));
  return [x, y, z] as const;
});

const connections: { from: number; to: number; distance: number }[] = [];

for (let from = 0; from < positions.length; from++) {
  const [fromX, fromY, fromZ] = positions[from];

  for (let to = from + 1; to < positions.length; to++) {
    const [toX, toY, toZ] = positions[to];
    const distance = Math.sqrt(
      (toX - fromX) ** 2 + (toY - fromY) ** 2 + (toZ - fromZ) ** 2,
    );
    connections.push({ from, to, distance });
  }
}

connections.sort((a, b) => a.distance - b.distance);

const componentIds = positions.map((_, index) => index);

let componentCount = componentIds.length;

for (let connectionId = 0; connectionId < connections.length; connectionId++) {
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
    const connectedComponents = Object.groupBy(componentIds, (id) => id);

    const connectedComponentSizes = Object.values(connectedComponents).map(
      (members) => members?.length || 0,
    );

    const part1 = connectedComponentSizes.sort((a, b) => b - a).slice(0, 3)
      .reduce((a, b) => a * b, 1);

    console.log(`Part 1: ${part1}`);
  }

  if (componentCount === 1) {
    console.log(`Part 2: ${positions[from][0] * positions[to][0]}`);
    break;
  }
}
