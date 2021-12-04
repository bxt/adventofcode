#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import {
  groupBy,
  maxWith,
  minWith,
} from "https://deno.land/std@0.116.0/collections/mod.ts";

type Board = number[][];

type Input = { boards: Board[]; draws: number[] };

const parseInput = (
  string: string,
): Input => {
  const [drawsString, ...boardsStrings] = string.trim().split(/\n\W*\n\W*/);
  const draws = drawsString.split(",").map((s) => parseInt(s, 10));

  const boards = boardsStrings.map((boardsString) =>
    boardsString.split(/\n\W*/).map((s) =>
      s.trim().split(/ +/).map((s) => parseInt(s, 10))
    )
  );

  return { boards, draws };
};

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

const part1 = ({ boards, draws }: Input): number => {
  const boardMarks: boolean[][][] = boards.map((a) =>
    a.map((b) => b.map(() => false))
  );

  for (const draw of draws) {
    for (let i = 0; i < boards.length; i++) {
      for (let j = 0; j < boards[i].length; j++) {
        for (let k = 0; k < boards[i][j].length; k++) {
          if (boards[i][j][k] === draw) {
            boardMarks[i][j][k] = true;

            if (
              boardMarks[i][j].every((b) => b) ||
              boardMarks[i].every((_, j2) => boardMarks[i][j2][k])
            ) {
              let score = 0;

              for (let l = 0; l < boards[i].length; l++) {
                for (let m = 0; m < boards[i][l].length; m++) {
                  if (!boardMarks[i][l][m]) score += boards[i][l][m];
                }
              }

              return draw * score;
            }
          }
        }
      }
    }

    // console.log(boardMarks);
    // console.log("##########");
  }

  throw new Error();
};

const example = parseInput(`
  7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

  22 13 17 11  0
   8  2 23  4 24
  21  9 14 16  7
   6 10  3 18  5
   1 12 20 15 19

   3 15  0  2 22
  9 18 13 17  5
  19  8  7 25 23
  20 11 10 24  4
  14 21 16 12  6

  14 21 17 24  4
  10 16 15  9 19
  18  8 23 26 20
  22 11 13  6  5
   2  0 12  3  7
`);

assertEquals(part1(example), 4512);

console.log("Result part 1: " + part1(input));
