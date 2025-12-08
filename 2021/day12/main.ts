#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import { sum } from "../../2020/utils.ts";

type Edge = [string, string];

const parseInput = (string: string): Edge[] =>
  string.trim().split("\n").map((line) => {
    const [a, b] = line.trim().split("-");
    return [a, b];
  });

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

const isBigCave = (string: string) => !!string.match(/^[A-Z]/);
assertEquals(isBigCave("start"), false);
assertEquals(isBigCave("end"), false);
assertEquals(isBigCave("A"), true);
assertEquals(isBigCave("a"), false);
assertEquals(isBigCave("c"), false);

const countPathsStartingAt = (
  cave: string,
  input: Edge[],
  visitedSmallCaves: Set<string>,
  canStillVisitExtraSmallCave: boolean,
): number => {
  if (cave === "end") return 1;

  const neighbors = [
    ...input.filter(([a, _]) => a === cave).map(([_, b]) => b),
    ...input.filter(([_, b]) => b === cave).map(([a, _]) => a),
  ];

  if (!isBigCave(cave)) {
    visitedSmallCaves = new Set([...visitedSmallCaves, cave]);
  }

  return sum(neighbors.map((nextCave) => {
    if (visitedSmallCaves.has(nextCave)) {
      if (
        canStillVisitExtraSmallCave && !["start", "end"].includes(nextCave)
      ) {
        return countPathsStartingAt(nextCave, input, visitedSmallCaves, false);
      } else {
        return 0;
      }
    } else {
      return countPathsStartingAt(
        nextCave,
        input,
        visitedSmallCaves,
        canStillVisitExtraSmallCave,
      );
    }
  }));
};

const countPaths = (input: Edge[], canVisitExtraSmallCave: boolean): number => {
  const visitedSmallCaves: Set<string> = new Set(["start"]);

  return countPathsStartingAt(
    "start",
    input,
    visitedSmallCaves,
    canVisitExtraSmallCave,
  );
};

const part1 = (input: Edge[]): number => {
  return countPaths(input, false);
};

const example1 = parseInput(`
  start-A
  start-b
  A-c
  A-b
  b-d
  A-end
  b-end
`);

assertEquals(part1(example1), 10);

const example2 = parseInput(`
  dc-end
  HN-start
  start-kj
  dc-start
  dc-HN
  LN-dc
  HN-end
  kj-sa
  kj-HN
  kj-dc
`);

assertEquals(part1(example2), 19);

const example3 = parseInput(`
  fs-end
  he-DX
  fs-he
  start-DX
  pj-DX
  end-zg
  zg-sl
  zg-pj
  pj-he
  RW-he
  fs-DX
  pj-RW
  zg-RW
  start-pj
  he-WI
  zg-he
  pj-fs
  start-RW
`);

assertEquals(part1(example3), 226);

console.log("Result part 1: " + part1(input));

const part2 = (input: Edge[]): number => {
  return countPaths(input, true);
};

assertEquals(part2(example1), 36);
assertEquals(part2(example2), 103);
assertEquals(part2(example3), 3509);

console.log("Result part 2: " + part2(input));
