#!/usr/bin/env deno run --allow-read
import {
  assert,
  assertEquals,
} from "https://deno.land/std@0.79.0/testing/asserts.ts";
import {
  addCoords,
  boundsOfCoords,
  Coord,
  CoordArray,
  ensureElementOf,
  indexWithCoord,
  matchGroups,
  product,
  range,
  rangeCoords,
  scaleCoord,
  SparseCoordArray,
  sum,
} from "../utils.ts";

const entries = ["#", "."] as const;
type Entry = typeof entries[number];

const TILE_SIZE = 10;
type TileId = number;
type Tile = { id: TileId; data: CoordArray<Entry> };

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

const tiles = parseInput(input);

const neighborCoordOffsets: Coord[] = [
  [+1, 0],
  [-1, 0],
  [0, +1],
  [0, -1],
];

const directionSelectors: SparseCoordArray<(tile: Tile) => Entry[]> = {
  "1": {
    "0": (tile) => tile.data[tile.data.length - 1],
  },
  "-1": {
    "0": (tile) => tile.data[0],
  },
  "0": {
    "1": (tile) => tile.data.map((row) => row[row.length - 1]),
    "-1": (tile) => tile.data.map((row) => row[0]),
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
  const reference = indexWithCoord(directionSelectors, direction)(tile);
  const oppositeDirection = scaleCoord(direction, -1);
  const directionSelector = indexWithCoord(
    directionSelectors,
    oppositeDirection,
  );

  for (let i = 0; i < transformations.length; i++) {
    if (entriesEqual(directionSelector(otherTile), reference)) return true;
    transformations[i](otherTile);
  }

  return false;
};

const positionsToCheck: [Coord, Tile][] = [[[0, 0], tiles[0]]];
const tilePositions: Record<TileId, Coord> = { [tiles[0].id]: [0, 0] };

while (positionsToCheck.length) {
  const positionToCheck = positionsToCheck.pop();
  assert(positionToCheck !== undefined);
  const [coord, tile] = positionToCheck;

  neighborCoordOffsets.forEach((neighborCoordOffset) => {
    tiles.forEach((neighborTile) => {
      if (tilePositions[neighborTile.id] !== undefined) return;
      const isMatching = matchUp(tile, neighborTile, neighborCoordOffset);
      if (isMatching) {
        const neighborCoord = addCoords(neighborCoordOffset, coord);
        tilePositions[neighborTile.id] = neighborCoord;
        positionsToCheck.push([neighborCoord, neighborTile]);
      }
    });
  });
}

assertEquals(Object.keys(tilePositions).length, tiles.length);

const [minCoord, maxCoord] = boundsOfCoords(Object.values(tilePositions));

const coordCorrection = scaleCoord(minCoord, -1);
const coordExtend = addCoords(addCoords(maxCoord, coordCorrection), [1, 1]);

const inverseTilePosition: SparseCoordArray<TileId> = {};
Object.entries(tilePositions).forEach((
  [tileId, coord],
) => {
  const correctedCoord = addCoords(coord, coordCorrection);
  inverseTilePosition[correctedCoord[1]] ||= {};
  inverseTilePosition[correctedCoord[1]][correctedCoord[0]] = Number(tileId);
});

const oppositeCorner = addCoords(coordExtend, [-1, -1]);
const cornerCoords: Coord[] = [
  [0, 0],
  [0, oppositeCorner[1]],
  [oppositeCorner[0], 0],
  oppositeCorner,
];
const cornerPieceIds = cornerCoords.map((coord) =>
  indexWithCoord(inverseTilePosition, coord)
);

assertEquals(cornerPieceIds.length, 4);

const part1 = product(cornerPieceIds);

console.log("Result part 1: " + part1);

const map = range(coordExtend[1]).flatMap((tileY) =>
  range(TILE_SIZE - 2).map((y) =>
    range(coordExtend[0]).flatMap((tileX) =>
      range(TILE_SIZE - 2).map((x) => {
        const tileCoord: Coord = [tileX, tileY];
        const tileId = indexWithCoord(inverseTilePosition, tileCoord);

        const tile = tiles.find((t) => t.id === Number(tileId));
        assert(tile !== undefined);

        const innerCoord: Coord = [x + 1, y + 1];
        return indexWithCoord(tile.data, innerCoord);
      })
    )
  )
);

const seaMonster = [
  "                  # ",
  "#    ##    ##    ###",
  " #  #  #  #  #  #   ",
];
const seaMonsterExtend: Coord = [seaMonster[0].length, seaMonster.length];
const seaMonsterCoords: Coord[] = rangeCoords(seaMonsterExtend).flatMap((
  [x, y],
) => (seaMonster[y].charAt(x) === "#") ? [[x, y]] : []);

assertEquals(seaMonsterCoords.length, 15);

const mapTile = { id: -1, data: map };

for (let i = 0; i < transformations.length; i++) {
  transformations[i](mapTile);
  const mapExtend: Coord = [mapTile.data[0].length, mapTile.data.length];

  const seaMonsterPostions: Coord[] = rangeCoords(
    addCoords(mapExtend, scaleCoord(seaMonsterExtend, -1)),
  ).flatMap((coord) =>
    seaMonsterCoords.every((seaMonsterCoord) =>
        indexWithCoord(
          mapTile.data,
          addCoords(coord, seaMonsterCoord),
        ) === "#"
      )
      ? [coord]
      : []
  );

  if (seaMonsterPostions.length > 0) {
    seaMonsterPostions.forEach(([x, y]) => {
      seaMonsterCoords.forEach(([sx, sy]) => {
        mapTile.data[y + sy][x + sx] = ".";
      });
    });

    const roughWaters = sum(
      mapTile.data.map((row) => row.filter((e) => e === "#").length),
    );

    console.log("Result part 2: " + roughWaters);
    break;
  }
}
