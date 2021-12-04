#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { minBy } from "https://deno.land/std@0.116.0/collections/mod.ts";
import { sum } from "../../2020/utils.ts";

type Board = number[][];
type BoardMarks = boolean[][];
type BoardWin = { drawIndex: number; score: number };
type Input = { boards: Board[]; draws: number[] };

const parseInput = (string: string): Input => {
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

const sumUnmarkedFields = (board: Board, boardMarks: BoardMarks): number => {
  return sum(
    board.flatMap((rowValues, row) =>
      rowValues.filter((_, col) => !boardMarks[row][col])
    ),
  );
};

const hasWonAt = (
  { boardMarks, row, col }: {
    boardMarks: BoardMarks;
    row: number;
    col: number;
  },
): boolean => {
  const rowIsFilled = boardMarks[row].every((b) => b);
  const columnIsFilled = boardMarks.every((_, row) => boardMarks[row][col]);
  return rowIsFilled || columnIsFilled;
};

const findBoardWin = (board: Board, draws: number[]): BoardWin => {
  const boardMarks: BoardMarks = board.map((b) => b.map(() => false));

  for (let drawIndex = 0; drawIndex < draws.length; drawIndex++) {
    const draw = draws[drawIndex];

    for (let row = 0; row < board.length; row++) {
      for (let col = 0; col < board[row].length; col++) {
        if (board[row][col] === draw) {
          boardMarks[row][col] = true;

          if (hasWonAt({ boardMarks, row, col })) {
            const score = sumUnmarkedFields(board, boardMarks);
            return { drawIndex, score };
          }
        }
      }
    }
  }

  throw new Error("Board does not win");
};

const getScoreOfMinBoardBy = ({ boards, draws, selector }: Input & {
  selector: (boardWin: BoardWin) => number;
}): number => {
  const boardWins = boards.map((board) => findBoardWin(board, draws));

  const selectedBoardWin = minBy(boardWins, selector);
  if (!selectedBoardWin) throw new Error("No board win selected");
  const { drawIndex, score } = selectedBoardWin;

  return draws[drawIndex] * score;
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

const part1 = (input: Input): number => {
  return getScoreOfMinBoardBy({
    ...input,
    selector: ({ drawIndex }) => drawIndex,
  });
};

assertEquals(part1(example), 4512);

console.log("Result part 1: " + part1(input));

const part2 = (input: Input): number => {
  return getScoreOfMinBoardBy({
    ...input,
    selector: ({ drawIndex }) => -drawIndex,
  });
};

assertEquals(part2(example), 1924);

console.log("Result part 2: " + part2(input));
