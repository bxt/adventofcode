// run with `deno run --allow-read=input.txt main.ts`

type Pair = [number, number];
type ClawMachine = {
  buttonA: Pair;
  buttonB: Pair;
  prize: Pair;
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
        buttonA: [parseInt(ax, 10), parseInt(ay, 10)],
        buttonB: [parseInt(bx, 10), parseInt(by, 10)],
        prize: [parseInt(px, 10), parseInt(py, 10)],
      };
    });
};

const file = await Deno.readTextFile("input.txt");

const parsedInput = parse(file);

const sum = (numbers: number[]): number => {
  return numbers.reduce((acc, number) => acc + number, 0);
};

const addScalar = ([x, y]: Pair, scalar: number): Pair => {
  return [x + scalar, y + scalar];
};

const getTokenSolution = (clawMachine: ClawMachine): number => {
  const { buttonA, buttonB, prize } = clawMachine;

  const bDividend = prize[1] * buttonA[0] - prize[0] * buttonA[1];
  const bDivisor = buttonB[1] * buttonA[0] - buttonB[0] * buttonA[1];

  if (bDividend % bDivisor !== 0) return 0;

  const b = bDividend / bDivisor;

  const a = (prize[0] - b * buttonB[0]) / buttonA[0];

  return a * 3 + b;
};

const tokensTotal = sum(parsedInput.map(getTokenSolution));

console.log(`Part 1: ${tokensTotal}`);

const adjustment = 10000000000000;

const adjusted = parsedInput.map((clawMachine): ClawMachine => {
  return {
    ...clawMachine,
    prize: addScalar(clawMachine.prize, adjustment),
  };
});

const tokensTotalAdjusted = sum(adjusted.map(getTokenSolution));

console.log(`Part 2: ${tokensTotalAdjusted}`);
