// run with `deno run --allow-read=input.txt main.ts`

type Position = readonly [number, number];

type Robot = {
  position: Position;
  velocity: Position;
};

const WIDTH = 101;
const HEIGHT = 103;
const STEPS = 100;

const regex = /^p=(?<px>\d+),(?<py>\d+) v=(?<vx>-?\d+),(?<vy>-?\d+)$/;

const parse = (input: string): Robot[] => {
  return input
    .trim()
    .split("\n")
    .map((line) => line.trim())
    .filter((l) => l)
    .map((line) => {
      const groups = line.match(regex)?.groups;
      if (!groups) throw new Error(`Does not match: ${line}`);
      const { px, py, vx, vy } = groups;
      const robot: Robot = {
        position: [parseInt(px, 10), parseInt(py, 10)],
        velocity: [parseInt(vx, 10), parseInt(vy, 10)],
      };
      return robot;
    });
};

const file = await Deno.readTextFile("input.txt");

const parsedInput = parse(file);

const quadrantCounts: [number, number, number, number] = [0, 0, 0, 0];

const midX = Math.floor(WIDTH / 2);
const midY = Math.floor(HEIGHT / 2);

for (const robot of parsedInput) {
  let quadrantIndex = 0;

  const finalX =
    (((robot.position[0] + robot.velocity[0] * STEPS) % WIDTH) + WIDTH) % WIDTH;
  if (finalX === midX) continue;
  quadrantIndex += Number(finalX > midX);

  const finalY =
    (((robot.position[1] + robot.velocity[1] * STEPS) % HEIGHT) + HEIGHT) %
    HEIGHT;
  if (finalY === midY) continue;
  quadrantIndex += Number(finalY > midY) * 2;

  quadrantCounts[quadrantIndex]++;
}

const safetyFactor = quadrantCounts.reduce(
  (accumulator, value) => accumulator * value,
  1
);

console.log(`Part 1: ${safetyFactor}`);
