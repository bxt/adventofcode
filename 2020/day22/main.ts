#!/usr/bin/env deno run --allow-read
import {
  assert,
  assertEquals,
} from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { matchGroups, StringifySinkSet, sum } from "../utils.ts";

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

assertEquals(example, copyGame(example));
assertEquals(parsedInput, copyGame(parsedInput));

const stringifyGame = (game: Game): string =>
  `Player 1:\n${game.player1.join("\n")}\nPlayer 2:\n${
    game.player2.join("\n")
  }\n`;

assertEquals(example, parseInput(stringifyGame(example)));
assertEquals(parsedInput, parseInput(stringifyGame(parsedInput)));

class GameSet extends StringifySinkSet<Game> {
  protected stringify(element: Game): string {
    return stringifyGame(element);
  }
}

const runGame = (
  game: Game,
  recurse: boolean,
): { winner: Player; score: number } => {
  const seen = new GameSet();

  const { player1, player2 } = game;

  while (player1.length > 0 && player2.length > 0) {
    if (seen.has(game)) {
      return { winner: 1, score: -1 }; // Player 1 wins
    }
    seen.add(game);

    const card1 = player1.shift();
    const card2 = player2.shift();
    assert(card1 !== undefined);
    assert(card2 !== undefined);

    const roundWinner: Player =
      (recurse && player1.length >= card1 && player2.length >= card2)
        ? runGame({
          player1: player1.slice(0, card1),
          player2: player2.slice(0, card2),
        }, recurse).winner
        : card1 > card2
        ? 1
        : 2;

    if (roundWinner === 1) {
      player1.push(card1);
      player1.push(card2);
    } else if (roundWinner === 2) {
      player2.push(card2);
      player2.push(card1);
    }
  }

  const winner = player1.length === 0 ? 2 : 1;
  const score = calculateScore([[], player1, player2][winner]);

  return { winner, score };
};

const part1 = (game: Game): number => {
  const copy = copyGame(game);
  return runGame(copy, false).score;
};

assertEquals(part1(example), 306);

console.log("Result part 1: " + part1(parsedInput));

const part2 = (game: Game): number => {
  const copy = copyGame(game);
  return runGame(copy, true).score;
};

assertEquals(part2(example), 291);

console.log("Result part 2: " + part2(parsedInput));
