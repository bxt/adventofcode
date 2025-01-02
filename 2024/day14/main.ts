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
      return {
        position: [parseInt(px, 10), parseInt(py, 10)],
        velocity: [parseInt(vx, 10), parseInt(vy, 10)],
      };
    });
};

const file = await Deno.readTextFile("input.txt");

const parsedInput = parse(file);

const quadrantCounts: [number, number, number, number] = [0, 0, 0, 0];

const midX = Math.floor(WIDTH / 2);
const midY = Math.floor(HEIGHT / 2);

const clamp = (size: number) => (value: number) =>
  ((value % size) + size) % size;
const clampX = clamp(WIDTH);
const clampY = clamp(HEIGHT);

const robotPositionAfter = (robot: Robot, steps: number): Position => {
  const finalX = clampX(robot.position[0] + robot.velocity[0] * steps);
  const finalY = clampY(robot.position[1] + robot.velocity[1] * steps);
  return [finalX, finalY];
};

for (const robot of parsedInput) {
  let quadrantIndex = 0;

  const [x, y] = robotPositionAfter(robot, STEPS);

  if (x === midX) continue;
  quadrantIndex += Number(x > midX);

  if (y === midY) continue;
  quadrantIndex += Number(y > midY) * 2;

  quadrantCounts[quadrantIndex]++;
}

const safetyFactor = quadrantCounts.reduce(
  (accumulator, value) => accumulator * value,
  1
);

console.log(`Part 1: ${safetyFactor}`);

let longestHorizontalLineSoFar = 0;

for (let steps = 0; steps < 10000; steps++) {
  const occupied = new Set<string>();

  for (const robot of parsedInput) {
    const position = robotPositionAfter(robot, steps);
    occupied.add(position.toString());
  }

  let longestHorizontalLine = 0;

  for (let y = 0; y < HEIGHT; y++) {
    let lineLength = 0;
    for (let x = 0; x < WIDTH; x++) {
      const position: Position = [x, y];
      const hasRobot = occupied.has(position.toString());
      if (hasRobot) {
        lineLength++;
        if (lineLength > longestHorizontalLine) {
          longestHorizontalLine = lineLength;
        }
      } else {
        lineLength = 0;
      }
    }
  }

  if (longestHorizontalLine < longestHorizontalLineSoFar) continue;

  longestHorizontalLineSoFar = longestHorizontalLine;

  console.log(`------`);
  console.log(`After ${steps} steps with line of ${longestHorizontalLine}:`);
  for (let y = 0; y < HEIGHT; y++) {
    for (let x = 0; x < WIDTH; x++) {
      const position: Position = [x, y];
      const hasRobot = occupied.has(position.toString());
      const icon = hasRobot ? "#" : ".";
      Deno.stdout.writeSync(new TextEncoder().encode(icon));
    }
    Deno.stdout.writeSync(new TextEncoder().encode("\n"));
  }
}
