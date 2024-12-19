// run with `deno run --allow-read=input.txt main.ts`

type Position = readonly [number, number];

const MAX = 70;
// const MAX = 6;

// const bytes = [
//   [5, 4],
//   [4, 2],
//   [4, 5],
//   [3, 0],
//   [2, 1],
//   [6, 3],
//   [2, 4],
//   [1, 5],
//   [0, 6],
//   [3, 3],
//   [2, 6],
//   [5, 1],
// ];

const parse = (input: string): Position[] => {
  return input
    .trim()
    .split("\n")
    .map((line) => {
      const [x, y] = line.trim().split(",");
      return [parseInt(x, 10), parseInt(y, 10)];
    });
};

const add = ([x1, y1]: Position, [x2, y2]: Position): Position => {
  return [x1 + x2, y1 + y2];
};

const equals = ([x1, y1]: Position, [x2, y2]: Position): boolean => {
  return x1 === x2 && y1 === y2;
};

const isInBounds = ([x, y]: Position): boolean => {
  return x >= 0 && x <= MAX && y >= 0 && y <= MAX;
};

const fourDirections = [
  [1, 0],
  [0, 1],
  [-1, 0],
  [0, -1],
] as const;

const file = await Deno.readTextFile("input.txt");

const parsedInput = parse(file);

const startPosition: Position = [0, 0];

const endPosition: Position = [MAX, MAX];

let frontier = [startPosition];
let steps = 0;

const fistKiloByte = new Set(parsedInput.slice(0, 1024).map((p) => p.toString()));

const seen = new Set([startPosition.toString()]);

while (true) {
  steps++;

  frontier = frontier.flatMap((position) =>
    fourDirections.map((direction) => add(position, direction))
  );
  frontier = frontier.filter(isInBounds);
  frontier = frontier.filter(
    (position) => !fistKiloByte.has(position.toString())
  );
  frontier = frontier.filter(
    (position, index) =>
      frontier.findLastIndex((p) => equals(p, position)) === index
  );
  frontier = frontier.filter((position) => !seen.has(position.toString()));

  frontier.forEach((position) => seen.add(position.toString()));

  // Deno.stdout.writeSync(new TextEncoder().encode(steps.toString()));
  // Deno.stdout.writeSync(new TextEncoder().encode('\n'));

  // for (let x = 0; x <= MAX; x++) {
  //   for (let y = 0; y <= MAX; y++) {
  //     if (frontier.find((p) => equals(p, [x, y]))) {
  //       Deno.stdout.writeSync(new TextEncoder().encode('O'));
  //     } else if (seen.has([x, y].toString())) {
  //       Deno.stdout.writeSync(new TextEncoder().encode('x'));
  //     } else if (fistKiloByte.has([x, y].toString())) {
  //       Deno.stdout.writeSync(new TextEncoder().encode('#'));
  //     } else {
  //       Deno.stdout.writeSync(new TextEncoder().encode('.'));
  //     }
  //   }
  //   Deno.stdout.writeSync(new TextEncoder().encode('\n'));
  // }
  // Deno.stdout.writeSync(new TextEncoder().encode('\n'));

  if (frontier.some((p) => equals(p, endPosition))) break;
}

console.log(`Part 1: ${steps}`);
