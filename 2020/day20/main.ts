#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";
import { ensureElementOf, matchGroups, product } from "../utils.ts";

const entries = ["#", "."] as const;
type Entry = typeof entries[number];

const TILE_SIZE = 10;
type TileId = number;
type Tile = { id: TileId; data: Entry[][] };

const parseInput = (string: string): Tile[] =>
  string.trim().split("\n\n").map((tileString) => {
    const { idString, dataString } = matchGroups(
      /Tile (?<idString>\d+):(?<dataString>(\n *[.#]{10}){10})/,
    )(tileString.trim());
    const id = Number(idString);
    const data = dataString.trim().split(/\n */).map((row) =>
      row.split("").map((e) => ensureElementOf(e, entries))
    );
    return { id, data };
  });

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

const borderIdUnidirectional = (entries: Entry[]): number =>
  parseInt(entries.map((e) => e === "#" ? 1 : 0).join(""), 2);

const borderId = (entries: Entry[]): number => {
  const id1 = borderIdUnidirectional(entries);
  const id2 = borderIdUnidirectional(entries.reverse());
  return Math.min(id1, id2);
};

const borderIds = (tile: Tile): number[] => {
  const borders = [
    tile.data[0],
    tile.data[TILE_SIZE - 1],
    tile.data.map((row) => row[0]),
    tile.data.map((row) => row[TILE_SIZE - 1]),
  ];
  return borders.map(borderId);
};

const part1 = (tiles: Tile[]): number => {
  const borderIdsCache: Record<TileId, number[]> = {};
  const matchCounts: Record<TileId, number> = {};

  const getBorderIds = (tile: Tile): number[] => {
    if (borderIdsCache[tile.id] !== undefined) return borderIdsCache[tile.id];
    const myBorderIds = borderIds(tile);
    borderIdsCache[tile.id] = myBorderIds;
    return myBorderIds;
  };

  for (let i = 0; i < tiles.length; i++) {
    matchCounts[tiles[i].id] = 0;
    for (let k = 0; k < tiles.length; k++) {
      if (i === k) continue;
      if (
        getBorderIds(tiles[i]).some((bid) =>
          getBorderIds(tiles[k]).includes(bid)
        )
      ) {
        matchCounts[tiles[i].id]++;
      }
    }
  }

  console.log({ matchCounts });

  console.log(
    Object.entries(matchCounts).reduce(
      (soFar, [_, v]) => ({
        ...soFar,
        [v]: soFar[v] === undefined ? 1 : soFar[v] + 1,
      }),
      {} as Record<number, number>,
    ),
  );

  const corners = Object.entries(matchCounts).filter(([k, v]) => v === 2).map((
    [k, v],
  ) => Number(k));
  assertEquals(corners.length, 4);

  return product(corners);
};

console.log("Result part 1: " + part1(inputParsed));
