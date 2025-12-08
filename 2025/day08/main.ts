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

const closestConnections = connections.slice(0, 1000);

console.log(closestConnections.length);

const seen = positions.map(() => false);
const connectedComponentSizes = [];

for (let from = 0; from < positions.length; from++) {
  if (seen[from]) continue;
  seen[from] = true;

  const queue = [from];
  let componentSize = 1;
  while (queue.length > 0) {
    const current = queue.pop();

    if (current === undefined) throw new Error("Unexpected empty item");

    for (const connection of closestConnections) {
      let to = undefined;
      if (connection.from === current) {
        to = connection.to;
      } else if (connection.to === current) {
        to = connection.from;
      }

      if (to === undefined) continue;

      if (seen[to]) continue;
      seen[to] = true;

      queue.push(to);
      componentSize++;
    }
  }

  connectedComponentSizes.push(componentSize);
}

const part1 = connectedComponentSizes.sort((a, b) => b - a).slice(0, 3).reduce(
  (a, b) => a * b,
  1,
);

console.log(`Part 1: ${part1}`);
