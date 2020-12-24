#!/usr/bin/env deno run --allow-read
import {
  assert,
  assertEquals,
} from "https://deno.land/std@0.79.0/testing/asserts.ts";
import {
  addCoords,
  boundsOfCoords,
  Coord,
  ensureElementOf,
  matchGroups,
  range,
  rangeCoords,
  sum,
} from "../utils.ts";

const origin = [0, 0] as Coord;

const directions = ["e", "w", "se", "sw", "ne", "nw"] as const;
type Direction = typeof directions[number];

type StringifiedCoord = string;

const parseInput = (string: string): Direction[][] => {
  return string.trim().split("\n").map((line) =>
    [...line.trim().matchAll(/[ns]?[we]/g)].map((d) =>
      ensureElementOf(d[0], directions)
    )
  );
};

const example1 = parseInput(`esew`);
const example2 = parseInput(`nwwswee`);
const example3 = parseInput(`
  sesenwnenenewseeswwswswwnenewsewsw
  neeenesenwnwwswnenewnwwsewnenwseswesw
  seswneswswsenwwnwse
  nwnwneseeswswnenewneswwnewseswneseene
  swweswneswnenwsewnwneneseenw
  eesenwseswswnenwswnwnwsewwnwsene
  sewnenenenesenwsewnenwwwse
  wenwwweseeeweswwwnwwe
  wsweesenenewnwwnwsenewsenwwsesesenwne
  neeswseenwwswnwswswnw
  nenwswwsewswnenenewsenwsenwnesesenew
  enewnwewneswsewnwswenweswnenwsenwsw
  sweneswneswneneenwnewenewwneswswnese
  swwesenesewenwneswnwwneseswwne
  enesenwswwswneneswsenwnewswseenwsese
  wnwnesenesenenwwnenwsewesewsesesew
  nenewswnwewswnenesenwnesewesw
  eneswnwswnwsenenwnwnwwseeswneewsenese
  neswnwewnwnwseenwseesewsenwsweewe
  wseweeenwnesenwwwswnew
`);

assertEquals(example1, [["e", "se", "w"]]);
assertEquals(example2, [["nw", "w", "sw", "e", "e"]]);
assertEquals(example3.length, 20);
assertEquals(example3[0].length, 20);
assertEquals(example3[0][0], "se");

const input = await Deno.readTextFile("input.txt");

const parsedInput = parseInput(input);

const stringifyCoord = (coord: Coord): StringifiedCoord => coord.join(",");
const parseCoord = (string: StringifiedCoord): Coord =>
  string.split(",").map(Number) as unknown as Coord;

rangeCoords([11, 11]).map((c) => addCoords(c, [-5, -5])).forEach((coord) => {
  assertEquals(coord, parseCoord(stringifyCoord(coord)));
  assertEquals(
    coord,
    parseCoord(
      stringifyCoord(
        parseCoord(stringifyCoord(parseCoord(stringifyCoord(coord)))),
      ),
    ),
  );
});

/**
 *     / \/ \      / \/ \
 *    |nw|ne|     |-+|*+|
 *   / \/ \/ \   / \/ \/ \
 *  | w|  |e |  |-*|**|+*|
 *  \ /\ /\ /   \ /\ /\ /
 *  |sw|se|     |*-|+-|
 *  \ /\ /      \ /\ /
 */
const walk = (coord: Coord, direction: Direction): Coord => {
  const [x, y] = coord;
  switch (direction) {
    case "e":
      return [x + 1, y];
    case "w":
      return [x - 1, y];
    case "se":
      return [x + 1, y - 1];
    case "sw":
      return [x, y - 1];
    case "ne":
      return [x, y + 1];
    case "nw":
      return [x - 1, y + 1];
  }
};

const walkAll = (coord: Coord, directions: Direction[]): Coord =>
  directions.reduce(walk, coord);

assertEquals(origin, walkAll(origin, ["nw", "w", "sw", "e", "e"]));
assertEquals(walkAll(origin, ["se"]), walkAll(origin, ["e", "se", "w"]));

const loadTiles = (directionsList: Direction[][]): Set<StringifiedCoord> => {
  const blackTiles = new Set<string>();
  directionsList.forEach((directions) => {
    const target = walkAll(origin, directions);
    const targetString = stringifyCoord(target);
    if (blackTiles.has(targetString)) {
      blackTiles.delete(targetString);
    } else {
      blackTiles.add(targetString);
    }
  });
  return blackTiles;
};

const part1 = (directionsList: Direction[][]): number => {
  const blackTiles = loadTiles(directionsList);
  return blackTiles.size;
};

assertEquals(part1(example1), 1);
assertEquals(part1(example2), 1);
assertEquals(part1(example3), 10);

console.log("Result part 1: " + part1(parsedInput));

const neighbors = (coord: Coord): Coord[] =>
  directions.map((direction) => walk(coord, direction));

const iterate = (blackTiles: Set<StringifiedCoord>): Set<StringifiedCoord> => {
  const tilesToCheck = new Set(blackTiles);
  blackTiles.forEach((coordString) => {
    neighbors(parseCoord(coordString)).forEach((neighbor) => {
      tilesToCheck.add(stringifyCoord(neighbor));
    });
  });

  const newBlackTiles = new Set<StringifiedCoord>();

  tilesToCheck.forEach((coordString) => {
    const blackNeighborCount =
      neighbors(parseCoord(coordString)).filter((neighbor) =>
        blackTiles.has(stringifyCoord(neighbor)) ? 1 : 0
      ).length;

    if (blackTiles.has(coordString)) {
      if (blackNeighborCount === 1 || blackNeighborCount === 2) {
        newBlackTiles.add(coordString);
      }
    } else {
      if (blackNeighborCount === 2) {
        newBlackTiles.add(coordString);
      }
    }
  });

  return newBlackTiles;
};

(() => {
  let blackTiles = loadTiles(example3);
  const counts: Record<number, number> = {
    1: 15,
    2: 12,
    3: 25,
    4: 14,
    5: 23,
    6: 28,
    7: 41,
    8: 37,
    9: 49,
    10: 37,
    20: 132,
    30: 259,
    40: 406,
    50: 566,
    60: 788,
    70: 1106,
    80: 1373,
    90: 1844,
    100: 2208,
  };
  for (let i = 0; i < 101; i++) {
    if (counts[i] !== undefined) {
      assertEquals(blackTiles.size, counts[i]);
    }
    blackTiles = iterate(blackTiles);
  }
})();

const part2 = (directionsList: Direction[][]): number => {
  let blackTiles = loadTiles(directionsList);
  for (let i = 0; i < 100; i++) {
    blackTiles = iterate(blackTiles);
  }
  return blackTiles.size;
};

assertEquals(part2(example3), 2208);

console.log("Result part 2: " + part2(parsedInput));
