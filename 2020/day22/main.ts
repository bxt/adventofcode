#!/usr/bin/env deno run --allow-read
import {
  assert,
  assertEquals,
} from "https://deno.land/std@0.79.0/testing/asserts.ts";
import { intersectSets, matchGroups, minusSets, sum } from "../utils.ts";

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

const part1 = (
  game: Game,
): number => {
  const { player1, player2 } = game;

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
