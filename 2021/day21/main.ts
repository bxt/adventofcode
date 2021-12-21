#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { matchGroups, range, sum } from "../../2020/utils.ts";

const matchInput = matchGroups(
  /Player \d+ starting position: (?<pos>\d+)/,
);

function parseInput(string: string): number[] {
  const lines = string.trim().split("\n");
  return lines.map((line) => parseInt(matchInput(line).pos, 10));
}

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

function part1(input: number[]): number {
  console.log({ input });
  const positions = [...input];
  const scores = input.map(() => 0);
  let dice = 1;
  const roll = () => (dice++ - 1) % 100 + 1;

  while (true) {
    for (let i = 0; i < positions.length; i++) {
      const forward = roll() + roll() + roll();
      positions[i] = (positions[i] + forward - 1) % 10 + 1;
      scores[i] += positions[i];
      if (scores[i] >= 1000) {
        const rolls = dice - 1;
        const looserScore = scores[(i + 1) % scores.length];
        return rolls * looserScore;
      }
    }
  }
}

const example = parseInput(`
  Player 1 starting position: 4
  Player 2 starting position: 8
`);

assertEquals(part1(example), 739785);

console.log("Result part 1: " + part1(input));

type State = {
  scores: number[];
  positions: number[];
  amount: number;
};

const threeRollsProbabilities: Record<number, number> = {};
for (let roll1 = 1; roll1 <= 3; roll1++) {
  for (let roll2 = 1; roll2 <= 3; roll2++) {
    for (let roll3 = 1; roll3 <= 3; roll3++) {
      const sum = roll1 + roll2 + roll3;
      threeRollsProbabilities[sum] ??= 0;
      threeRollsProbabilities[sum]++;
    }
  }
}
// for (let roll1 = 1; roll1 <= 3; roll1++) {
//   const sum = roll1;
//   threeRollsProbabilities[sum] ??= 0;
//   threeRollsProbabilities[sum]++;
// }

console.log({ threeRollsProbabilities });

function numbersOfWins(positions: number[]): number[] {
  // roll/*/state
  const statesAfterRolls: State[][] = [
    [{ scores: [0, 0], positions: positions, amount: 1 }],
  ];

  for (let roll = 1; roll <= 21 * 2; roll++) {
    const currentPlayer = (roll - 1) % 2;
    const otherPlayer = roll % 2;
    const prevStates = statesAfterRolls[roll - 1];

    const nextStates: State[] = [];
    for (
      const [nextRollString, nextRollAmount] of Object.entries(
        threeRollsProbabilities,
      )
    ) {
      const nextRoll = parseInt(nextRollString, 10);
      for (
        const { scores: oldScores, positions: oldPositions, amount: oldAmount }
          of prevStates
      ) {
        if (oldScores[otherPlayer] >= 21) continue;
        const newPositions = [...oldPositions];
        const newScores = [...oldScores];
        newPositions[currentPlayer] =
          (oldPositions[currentPlayer] + nextRoll - 1) % 10 +
          1;
        newScores[currentPlayer] = oldScores[currentPlayer] +
          newPositions[currentPlayer];
        let state = nextStates.find(({ scores, positions }) =>
          scores[0] === newScores[0] && positions[0] === newPositions[0] &&
          scores[1] === newScores[1] && positions[1] === newPositions[1]
        );
        if (state === undefined) {
          state = { scores: newScores, positions: newPositions, amount: 0 };
          nextStates.push(state);
        }
        state.amount += oldAmount * nextRollAmount;
      }
    }

    statesAfterRolls[roll] = nextStates;
  }

  statesAfterRolls.forEach((s, i) => {
    console.log(`##### Roll ${i} ######`);
    console.log(s);
  });

  return positions.map((_, i) =>
    sum(
      statesAfterRolls.map((s) =>
        sum(
          s.filter(({ scores }) => scores[i] >= 21).map(({ amount }) => amount),
        )
      ),
    )
  );
}

function part2(input: number[]): number {
  console.log({ input });
  const positions = [...input];
  return Math.max(...numbersOfWins(positions));
}

assertEquals(part2(example), 444356092776315);

console.log("Result part 2: " + part2(input));
