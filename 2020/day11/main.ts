#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import {
  addCoords,
  Coord,
  CoordArray,
  indexWithCoord,
  scaleCoord,
  sum,
} from "../utils.ts";

enum Cell {
  Empty = "L",
  Occupied = "#",
  Floor = ".",
}

const parseInput = (string: string): CoordArray<Cell> =>
  string.trim().split(/[\n ]+/)
    .map((line) =>
      line.split("").map((l) =>
        l === "L" ? Cell.Empty : l === "#" ? Cell.Occupied : Cell.Floor
      )
    );

const example = parseInput(`
  L.LL.LL.LL
  LLLLLLL.LL
  L.L.L..L..
  LLLL.LL.LL
  L.LL.LL.LL
  L.LLLLL.LL
  ..L.L.....
  LLLLLLLLLL
  L.LLLLLL.L
  L.LLLLL.LL
`);

assertEquals(example[0][0], Cell.Empty);
assertEquals(example[0][1], Cell.Floor);
assertEquals(example.length, 10);
assertEquals(example[0].length, 10);

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

const neighborDirections: Coord[] = [
  [-1, -1],
  [+0, -1],
  [+1, -1],
  [-1, +0],
  [+1, +0],
  [-1, +1],
  [+0, +1],
  [+1, +1],
];

const countOccupied = (cellList: Cell[]): number =>
  cellList.filter((c) => c === Cell.Occupied).length;

class Game {
  constructor(
    private cells: CoordArray<Cell>,
    private readonly minOccupiedToBecomeEmpty: number,
    private readonly maxDistance?: number,
  ) {
  }

  private isInBounds([x, y]: Coord): boolean {
    return y >= 0 && y < this.cells.length && x >= 0 &&
      x < this.cells[y].length;
  }

  private getCellValue(coord: Coord): Cell {
    return indexWithCoord(this.cells, coord);
  }

  private getNeighborValues(coord: Coord): Cell[] {
    return neighborDirections.map((direction) => {
      for (
        let distance = 1;
        this.maxDistance === undefined || distance <= this.maxDistance;
        distance++
      ) {
        const potentialNeighbor = addCoords(
          coord,
          scaleCoord(direction, distance),
        );
        if (!this.isInBounds(potentialNeighbor)) return Cell.Floor;
        const cell = this.getCellValue(potentialNeighbor);
        if (cell !== Cell.Floor) return cell; // found a visible one
      }

      return Cell.Floor; // reached max distance
    });
  }

  private getNextCellValueAt(coord: Coord): Cell {
    const neighborValues = this.getNeighborValues(coord);
    const occupiedCount = countOccupied(neighborValues);
    const cell = this.getCellValue(coord);

    if (cell === Cell.Empty && occupiedCount === 0) {
      return Cell.Occupied;
    }
    if (
      cell === Cell.Occupied && occupiedCount >= this.minOccupiedToBecomeEmpty
    ) {
      return Cell.Empty;
    }

    return cell;
  }

  private next(): boolean {
    let changed = false;
    const newCells = this.cells.map((line, y) =>
      line.map((cell, x) => {
        const coord = [x, y] as const;
        const newCell = this.getNextCellValueAt(coord);
        if (newCell !== cell) changed = true;
        return newCell;
      })
    );
    this.cells = newCells;

    return changed;
  }

  iterateAll(): void {
    while (this.next());
  }

  getOccupiedCount(): number {
    return sum(this.cells.map((line) => countOccupied(line)));
  }
}

const runGame = (game: Game): number => {
  game.iterateAll();
  return game.getOccupiedCount();
};

const part1 = (cells: CoordArray<Cell>): number =>
  runGame(new Game(cells, 4, 1));

assertEquals(part1(example), 37);

console.log("Result part 1: " + part1(inputParsed));

const part2 = (cells: CoordArray<Cell>) => runGame(new Game(cells, 5));

assertEquals(part2(example), 26);

console.log("Result part 2: " + part2(inputParsed));
