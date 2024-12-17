// run with `deno run --allow-read=input.txt main.ts`

type Position = readonly [number, number];
type ClawMachine = {
  buttonA: Position;
  buttonB: Position;
  prize: Position;
};

const regex =
  /Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)/;

const parse = (input: string): ClawMachine[] => {
  return input
    .trim()
    .split("\n\n")
    .map((block) => {
      const match = regex.exec(block);
      if (!match) throw new Error(`Invalid block: ${block}`);
      const [, ax, ay, bx, by, px, py] = match;
      return {
        buttonA: [parseInt(ax), parseInt(ay)] as Position,
        buttonB: [parseInt(bx), parseInt(by)] as Position,
        prize: [parseInt(px), parseInt(py)] as Position,
      };
    });
};

const file = await Deno.readTextFile("input.txt");

const parsedInput = parse(file);

const getTokens = ([a, b]: Position): number => a * 3 + b;

let tokensTotal = 0;

const combinations = Array.from({ length: 100 })
  .flatMap((_, a) => Array.from({ length: 100 }, (_, b) => [a, b] as const))
  .sort((x, y) => {
    return getTokens(x) - getTokens(y);
  });

for (const clawMachine of parsedInput) {
  const { buttonA, buttonB, prize } = clawMachine;
  for (const combination of combinations) {
    const [a, b] = combination;
    if (
      a * buttonA[0] + b * buttonB[0] === prize[0] &&
      a * buttonA[1] + b * buttonB[1] === prize[1]
    ) {
      tokensTotal += getTokens(combination);
      break;
    }
  }
}

console.log(`Part 1: ${tokensTotal}`);

console.log(`Part 2: ${"???"}`);
