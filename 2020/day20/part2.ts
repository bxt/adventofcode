#!/usr/bin/env deno run --allow-read
import {
  assert,
  assertEquals,
} from "https://deno.land/std@0.79.0/testing/asserts.ts";
import {
  addCoords,
  Coord,
  ensureElementOf,
  matchGroups,
  product,
  scaleCoord,
  sum,
} from "../utils.ts";

const range = (length: number): number[] =>
  Array(length).fill(null).map((_, n) => n);

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

const neighbourCoordOffsets: Coord[] = [
  [+1, 0],
  [-1, 0],
  [0, +1],
  [0, -1],
];

const directionSelectors: Record<
  number,
  Record<number, (tile: Tile) => Entry[]>
> = {
  "1": {
    "0": (tile) => tile.data.map((row) => row[TILE_SIZE - 1]),
  },
  "-1": {
    "0": (tile) => tile.data.map((row) => row[0]),
  },
  "0": {
    "1": (tile) => tile.data[TILE_SIZE - 1],
    "-1": (tile) => tile.data[0],
  },
};

const entriesEqual = (a: Entry[], b: Entry[]): boolean =>
  a.every((e, i) => b[i] === e);

type Transformation = (tile: Tile) => void;

const nothing: Transformation = (_): void => {};

const rotate: Transformation = (tile) => {
  tile.data = range(tile.data[0].length).map((y) =>
    range(tile.data.length).map((x) => tile.data[tile.data.length - x - 1][y])
  );
};
const flip: Transformation = (tile) => {
  tile.data = tile.data.map((row) => row.reverse());
};

const transformations: Transformation[] = [
  nothing,
  rotate,
  rotate,
  rotate,
  flip,
  rotate,
  rotate,
  rotate,
];

const matchUp = (tile: Tile, otherTile: Tile, direction: Coord): boolean => {
  const reference = directionSelectors[direction[0]][direction[1]](tile);
  const oppositeDirection = scaleCoord(direction, -1);
  const directionSelector =
    directionSelectors[oppositeDirection[0]][oppositeDirection[1]];

  for (let i = 0; i < transformations.length; i++) {
    if (entriesEqual(directionSelector(otherTile), reference)) return true;
    transformations[i](otherTile);
  }

  return false;
};

const part2 = (tiles: Tile[]): number => {
  const positionsToCheck: [Coord, Tile][] = [[[0, 0], tiles[0]]];
  const tilePositions: Record<TileId, Coord> = { [tiles[0].id]: [0, 0] };

  while (positionsToCheck.length) {
    const positionToCheck = positionsToCheck.pop();
    assert(positionToCheck !== undefined);
    const [coord, tile] = positionToCheck;

    neighbourCoordOffsets.forEach((neighbourCoordOffset) => {
      tiles.forEach((otherTile) => {
        if (tilePositions[otherTile.id] !== undefined) return;
        const isMatching = matchUp(tile, otherTile, neighbourCoordOffset);
        if (isMatching) {
          const neighbourCoord = addCoords(neighbourCoordOffset, coord);
          tilePositions[otherTile.id] = neighbourCoord;
          positionsToCheck.push([neighbourCoord, otherTile]);
        }
      });
    });
  }

  console.log({ tilePositions });

  const coordCorrection = scaleCoord([
    Math.min(...Object.values(tilePositions).map((coord) => coord[0])),
    Math.min(...Object.values(tilePositions).map((coord) => coord[1])),
  ], -1);
  const coordExtend = addCoords(
    addCoords([
      Math.max(...Object.values(tilePositions).map((coord) => coord[0])),
      Math.max(...Object.values(tilePositions).map((coord) => coord[1])),
    ], coordCorrection),
    [1, 1],
  );

  const correctedTilePositions: Record<TileId, Coord> = Object.fromEntries(
    Object.entries(tilePositions).map((
      [tileId, coord],
    ) => [tileId, addCoords(coord, coordCorrection)]),
  );

  console.log({ correctedTilePositions });

  console.log(
    range(coordExtend[1]).map((y) =>
      range(coordExtend[0]).map((x) => {
        return Object.values(correctedTilePositions).find(([rx, ry]) =>
            rx === x && ry === y
          )
          ? "#"
          : ".";
      }).join("")
    ).join("\n"),
  );

  const cornerPieces = Object.entries(correctedTilePositions).filter((
    [_, coord],
  ) =>
    coord[0] === 0 && coord[1] === 0 ||
    coord[0] === 0 && coord[1] === coordExtend[1] - 1 ||
    coord[0] === coordExtend[0] - 1 && coord[1] === 0 ||
    coord[0] === coordExtend[0] - 1 && coord[1] === coordExtend[1] - 1
  );
  console.log({ cornerPieces });

  const part1 = product(
    cornerPieces.map(([tileId, _]) => Number(tileId)),
  );

  console.log("Result part 1: " + part1);

  const map = range(coordExtend[1]).flatMap((tileY) =>
    range(TILE_SIZE - 2).map((y) =>
      range(coordExtend[0]).flatMap((tileX) =>
        range(TILE_SIZE - 2).map((x) => {
          const tileId = Object.entries(correctedTilePositions).find((
            [_, [x, y]],
          ) => tileX === x && tileY === y)?.[0];
          assert(tileId !== undefined); // a gap?
          const tile = tiles.find((t) => t.id === Number(tileId));
          assert(tile !== undefined);
          return tile.data[y + 1][x + 1];
        })
      )
    )
  );

  console.log({ map });

  const seaMonster = [
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   ",
  ];
  const seaMonsterCoords = range(seaMonster.length).flatMap((y) =>
    range(seaMonster[y].length).flatMap((x) =>
      (seaMonster[y].charAt(x) === "#") ? [[x, y]] : []
    )
  );

  console.log({ seaMonsterCoords });

  const mapTile = { id: -1, data: map };

  for (let i = 0; i < transformations.length; i++) {
    transformations[i](mapTile);
    const seaMonsterPostions: Coord[] = [];

    range(mapTile.data.length - seaMonster.length).forEach((y) => {
      range(mapTile.data[y].length - seaMonster[0].length).forEach((x) => {
        if (
          seaMonsterCoords.every(([sx, sy]) => {
            return mapTile.data[y + sy][x + sx] === "#";
          })
        ) {
          seaMonsterPostions.push([x, y]);
        }
      });
    });

    if (seaMonsterPostions.length > 0) {
      console.log({ seaMonsterPostions });

      seaMonsterPostions.forEach(([x, y]) => {
        seaMonsterCoords.forEach(([sx, sy]) => {
          return mapTile.data[y + sy][x + sx] = ".";
        });
      });

      const roughWaters = sum(
        mapTile.data.map((row) => row.filter((e) => e === "#").length),
      );

      return roughWaters;
    }
  }

  return -1;
};

console.log("Result part 2: " + part2(inputParsed));
