#!/usr/bin/env deno run --allow-read
import {
  assert,
  assertEquals,
} from "https://deno.land/std@0.79.0/testing/asserts.ts";
import { intersectSets, matchGroups, minusSets, sum } from "../utils.ts";

type Player = 1 | 2;
type Card = number;
type Game = { player1: Card[]; player2: Card[] };

const parseInput = (string: string): Game => {
  const { player1String, player2String } = matchGroups(
    /Player 1:(?<player1String>[^]*)Player 2:(?<player2String>[^]*)/,
  )(string.trim());
  const [player1, player2] = [player1String, player2String].map(
    (playerString) =>
      playerString.trim().split("\n").map((line) => Number(line.trim())),
  );
  return { player1, player2 };
};

const example = parseInput(`
  Player 1:
  9
  2
  6
  3
  1

  Player 2:
  5
  8
  4
  7
  10
`);

assertEquals(example, { player1: [9, 2, 6, 3, 1], player2: [5, 8, 4, 7, 10] });

const input = await Deno.readTextFile("input.txt");

const parsedInput = parseInput(input);

const calculateScore = (deck: Card[]) => {
  return sum(deck.reverse().map((n, i) => n * (i + 1)));
};

assertEquals(calculateScore([3, 2, 10, 6, 8, 5, 9, 4, 7, 1]), 306);

const copyGame = (game: Game): Game => {
  return { player1: [...game.player1], player2: [...game.player2] };
};

const part1 = (game: Game): number => {
  const { player1, player2 } = copyGame(game);

  while (player1.length > 0 && player2.length > 0) {
    const card1 = player1.shift();
    const card2 = player2.shift();
    assert(card1 !== undefined);
    assert(card2 !== undefined);

    if (card1 > card2) {
      // console.log("Player 1 wins the round!");
      player1.push(card1);
      player1.push(card2);
    } else {
      // console.log("Player 2 wins the round!");
      player2.push(card2);
      player2.push(card1);
    }

    // console.log({ player1, player2 });
  }

  return calculateScore(player1.length === 0 ? player2 : player1);
};

assertEquals(part1(example), 306);

console.log("Result part 1: " + part1(parsedInput));

const stringifyGame = (game: Game): string =>
  `Player 1:\n${game.player1.join("\n")}\nPlayer 2:\n${
    game.player2.join("\n")
  }\n`;

assertEquals(example, parseInput(stringifyGame(example)));

const part2 = (
  game: Game,
  gameNumber?: { n: number },
): { winner: Player; score: number } => {
  const seen: Set<string> = new Set();

  gameNumber ||= { n: 0 };
  gameNumber.n++;
  const myGameNumber = gameNumber.n;

  // console.log(`=== Game ${myGameNumber} ===\n`);

  const { player1, player2 } = copyGame(game);

  let round = 0;

  while (player1.length > 0 && player2.length > 0) {
    const stringifiedGame = stringifyGame({ player1, player2 });
    if (seen.has(stringifiedGame)) {
      // console.log("Infinite loop prevention kicking in...");
      return { winner: 1, score: -1 }; // Player 1 wins
    }
    seen.add(stringifiedGame);

    round++;
    // console.log(`-- Round ${round} (Game ${myGameNumber}) --`);
    // console.log(`Player 1's deck: ${player1.join(", ")}`);
    // console.log(`Player 2's deck: ${player2.join(", ")}`);

    const card1 = player1.shift();
    const card2 = player2.shift();
    assert(card1 !== undefined);
    assert(card2 !== undefined);

    // console.log(`Player 1 plays: ${card1}`);
    // console.log(`Player 2 plays: ${card2}`);

    let winner: Player;
    if (player1.length >= card1 && player2.length >= card2) {
      // console.log("Playing a sub-game to determine the winner...\n");
      winner = part2(
        {
          player1: player1.slice(0, card1),
          player2: player2.slice(0, card2),
        },
        gameNumber,
      ).winner;
      // console.log(`...anyway, back to game ${myGameNumber}.`);
    } else {
      winner = card1 > card2 ? 1 : 2;
    }

    if (winner === 1) {
      player1.push(card1);
      player1.push(card2);
    } else if (winner === 2) {
      player2.push(card2);
      player2.push(card1);
    }

    // console.log(
    //   `Player ${winner} wins round ${round} of game ${myGameNumber}!\n`,
    // );
  }

  const winner = player1.length === 0 ? 2 : 1;
  const score = calculateScore([[], player1, player2][winner]);

  // console.log(`The winner of game ${myGameNumber} is player ${winner}!\n`);

  return { winner, score };
};

assertEquals(part2(example).score, 291);

console.log("Result part 2: " + part2(parsedInput).score);
