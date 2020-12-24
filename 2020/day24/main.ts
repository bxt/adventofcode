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

class StringifySet<T> implements Set<T> {
  #stringify: (element: T) => string;
  #parse: (string: string) => T;
  #set: Set<string>;

  constructor(
    stringify: (element: T) => string,
    parse: (string: string) => T,
    elements?: Iterable<T>,
  ) {
    this.#stringify = stringify;
    this.#parse = parse;
    if (elements) {
      this.#set = new Set([...elements].map(stringify));
    } else {
      this.#set = new Set();
    }
  }

  *[Symbol.iterator](): IterableIterator<T> {
    for (const stringItem of this.#set) {
      yield this.#parse(stringItem);
    }
  }

  get [Symbol.toStringTag](): string {
    return `[StringifySet ${this.#set}]`;
  }

  add(element: T): this {
    this.#set.add(this.#stringify(element));
    return this;
  }

  has(element: T): boolean {
    return this.#set.has(this.#stringify(element));
  }

  delete(element: T): boolean {
    return this.#set.delete(this.#stringify(element));
  }

  get size(): number {
    return this.#set.size;
  }

  forEach(callback: (element: T, element2: T, set: Set<T>) => void) {
    for (const item of this) {
      callback(item, item, this);
    }
  }

  clear(): void {
    this.#set.clear();
  }

  *entries(): IterableIterator<[T, T]> {
    for (const stringItem of this.#set) {
      const item = this.#parse(stringItem);
      yield [item, item];
    }
  }

  keys(): IterableIterator<T> {
    return this[Symbol.iterator]();
  }

  values(): IterableIterator<T> {
    return this[Symbol.iterator]();
  }
}

const stringifyCoord = (coord: Coord): string => coord.join(",");
const parseCoord = (string: string): Coord =>
  string.split(",").map(Number) as unknown as Coord;

const makeCoordSet = (elements?: Iterable<Coord>) =>
  new StringifySet(stringifyCoord, parseCoord, elements);

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

const loadTiles = (directionsList: Direction[][]): StringifySet<Coord> => {
  const blackTiles = makeCoordSet();
  directionsList.forEach((directions) => {
    const target = walkAll(origin, directions);
    if (blackTiles.has(target)) {
      blackTiles.delete(target);
    } else {
      blackTiles.add(target);
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

const iterate = (blackTiles: StringifySet<Coord>): StringifySet<Coord> => {
  const tilesToCheck = makeCoordSet(blackTiles);
  blackTiles.forEach((coord) => {
    neighbors(coord).forEach((neighbor) => {
      tilesToCheck.add(neighbor);
    });
  });

  const newBlackTiles = makeCoordSet();

  tilesToCheck.forEach((coord) => {
    const blackNeighborCount = neighbors(coord).filter((neighbor) =>
      blackTiles.has(neighbor) ? 1 : 0
    ).length;

    if (blackTiles.has(coord)) {
      if (blackNeighborCount === 1 || blackNeighborCount === 2) {
        newBlackTiles.add(coord);
      }
    } else {
      if (blackNeighborCount === 2) {
        newBlackTiles.add(coord);
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
