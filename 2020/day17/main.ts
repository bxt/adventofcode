#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";
import { ensureElementOf, sum } from "../utils.ts";

// SUSI OFFICIALLY ENTERED THE BUILDING! https://www.twitch.tv/veloxxmusic

const ACTIVE = "#";
const INACTIVE = ".";
type EntryValue = typeof ACTIVE | typeof INACTIVE;
type EntryContainer = {
  size: () => number;
  getAt: (at: number) => Entry;
};
type Entry = EntryValue | EntryContainer;

const makeArrayEntry = (array: Entry[]): Entry => {
  return {
    getAt: (at) => array[at],
    size: () => array.length,
  };
};

const increaseDimension = (entry: Entry): Entry => makeArrayEntry([entry]);

const parseInput = (string: string): Entry =>
  makeArrayEntry(
    string.trim().split("\n").map((
      line,
    ) =>
      makeArrayEntry(
        line.trim().split("").map((letter) =>
          ensureElementOf(letter, [ACTIVE, INACTIVE] as const)
        ),
      )
    ),
  );

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

const example = parseInput(`
  .#.
  ..#
  ###
`);

const next = (prevs: Entry[], current: Entry, nexts: Entry[]): Entry => {
  const activeNeighbours = prevs.filter((p) => p === ACTIVE).length +
    nexts.filter((p) => p === ACTIVE).length;
  switch (current) {
    case ACTIVE:
      return (activeNeighbours === 2 || activeNeighbours === 3)
        ? ACTIVE
        : INACTIVE;
    case INACTIVE:
      return (activeNeighbours === 3) ? ACTIVE : INACTIVE;
    default: {
      const result: Entry[] = [];
      for (let i = -1; i < current.size() + 1; i++) {
        result.push(
          next(
            prevs.map((p) => (p as EntryContainer).getAt(i - 1)),
            current.getAt(i),
            nexts.map((n) => (n as EntryContainer).getAt(i + 1)),
          ),
        );
      }
      return makeArrayEntry(result);
    }
  }
};

assertEquals(
  next([example])[0],
  parseInput(`
    .....
    .....
    .#...
    ...#.
    ..#..
  `),
);

assertEquals(
  next([example])[1],
  parseInput(`
    .....
    .....
    .#.#.
    ..##.
    ..#..
  `),
);

assertEquals(
  next([example])[2],
  parseInput(`
    .....
    .....
    .#...
    ...#.
    ..#..
  `),
);

const part1 = (input: Entry[][]) => {
  const finalPocketDimension = Array(6).fill(null).reduce(
    (pocketDimension: Entry[][][], _) => next(pocketDimension),
    [input],
  );
  return sum(
    finalPocketDimension.flatMap((slice) =>
      slice.flatMap((row) => row.flatMap((e) => e === ACTIVE ? 1 : 0))
    ),
  );
};

assertEquals(part1(example), 112);

console.log("Result part 1: " + part1(inputParsed));
