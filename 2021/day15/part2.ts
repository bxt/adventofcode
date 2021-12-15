#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { minBy } from "https://deno.land/std@0.116.0/collections/mod.ts";
import {
  addCoords,
  Coord,
  manhattanNormCoord,
  minMax,
  sum,
} from "../../2020/utils.ts";

const parseInput = (string: string): number[][] => {
  return string.trim().split("\n").map((line) =>
    line.trim().split("").map((s) => parseInt(s, 10))
  );
};

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

class Heap<T> {
  array: T[] = [];
  compareBy: (t: T) => number;

  constructor(compareBy: (t: T) => number) {
    this.compareBy = compareBy;
  }

  pop() {
    if (this.array.length <= 0) throw new Error("Nothing left");
    const max = this.array.shift();
    if (this.array.length > 0) {
      this.heapyfy(0);
    }
    return max;
  }

  push(element: T) {
    this.array.push(element);
    this.decreased(this.array.length - 1);
  }

  updates(element: T, transaction: () => void) {
    const index = this.searchIndex(element);
    // console.log({ a: this.array, index, element });
    transaction();
    if (index !== undefined) {
      this.heapyfy(index);
      this.decreased(index);
    } else {
      this.push(element);
    }
  }

  get size() {
    return this.array.length;
  }

  swap(i1: number, i2: number) {
    const tmp = this.array[i1];
    this.array[i1] = this.array[i2];
    this.array[i2] = tmp;
  }

  compare(i1: number, i2: number) {
    return this.compareBy(this.array[i1]) - this.compareBy(this.array[i2]);
  }

  parent_index(i: number) {
    return Math.floor((i - 1) / 2);
  }

  leftIndex(i: number) {
    return i * 2 + 1;
  }

  rightIndex(i: number) {
    return i * 2 + 2;
  }

  heapyfy(i: number) {
    const smallest = minBy(
      [i, this.leftIndex(i), this.rightIndex(i)].filter((i) =>
        i < this.array.length
      ),
      (i) => this.compareBy(this.array[i]),
    );
    if (smallest === undefined) throw new Error();
    if (smallest !== i) {
      this.swap(i, smallest);
      this.heapyfy(smallest);
    }
  }

  decreased(i: number) {
    while (
      i > 0 &&
      this.compareBy(this.array[i]) <
        this.compareBy(this.array[this.parent_index(i)])
    ) {
      this.swap(i, this.parent_index(i));
      i = this.parent_index(i);
    }
  }

  searchIndex(element: T, i = 0): number | undefined {
    // console.log("searchIndex", { element, i });
    if (i >= this.array.length) return;
    if (this.array[i] === element) return i;
    if (
      this.compareBy(element) < this.compareBy(this.array[i])
    ) {
      return;
    }
    return this.searchIndex(element, this.leftIndex(i)) ??
      this.searchIndex(element, this.rightIndex(i));
  }
}

{
  const heap = new Heap<{ value: number }>((n) => n.value);
  heap.push({ value: 23 });
  heap.push({ value: 11 });
  heap.push({ value: 56 });
  heap.push({ value: 2 });
  assertEquals(heap.pop(), { value: 2 });
  assertEquals(heap.pop(), { value: 11 });
  assertEquals(heap.pop(), { value: 23 });
  assertEquals(heap.pop(), { value: 56 });
}
{
  const heap = new Heap<{ value: number }>((n) => n.value);
  const e = { value: 23 };
  heap.push(e);
  heap.push({ value: 11 });
  heap.push({ value: 56 });
  heap.push({ value: 2 });
  heap.updates(e, () => {
    e.value = 6;
  });
  assertEquals(heap.pop(), { value: 2 });
  assertEquals(heap.pop(), { value: 6 });
  assertEquals(heap.pop(), { value: 11 });
  assertEquals(heap.pop(), { value: 56 });
}

const parseCoord = (s: string): Coord => {
  const [a, b] = s.split(",");
  return [parseInt(a, 10), parseInt(b, 10)];
};

const neighborCoords: Coord[] = [[1, 0], [0, 1], [-1, 0], [0, -1]];

const aStar = (field: number[][]) => {
  const width = field[0].length;
  const height = field.length;

  const start = "0,0";
  const predecessors: Record<string, string> = {};
  const knownDistances: Record<string, number> = {};

  const bestDistances: Record<string, number> = {}; // Hash.new { Float::INFINITY }
  bestDistances[start] = 0;

  const queue = new Heap<string>((element) =>
    bestDistances[element] + manhattanNormCoord(parseCoord(element))
  );
  queue.push(start);

  const handleChildren = (current: string) => {
    const currentCoord = parseCoord(current);
    const neighbours: [string, number][] = neighborCoords.map((nc) => {
      return addCoords(nc, currentCoord);
    }).filter(([x, y]) => (
      x < width * 5 && y < height * 5 && x >= 0 && y >= 0
    )).map(([x, y]) => {
      const originalValue = field[y % height][x % width];
      const adjustedValue =
        (originalValue + Math.floor(y / height) + Math.floor(x / width) - 1) %
          9 + 1;
      return [`${x},${y}`, adjustedValue];
    });

    for (const [neighbour, distance] of neighbours) {
      if (knownDistances[neighbour] === undefined) {
        const possibleDistance = knownDistances[current] + distance;
        const bestDistance = bestDistances[neighbour] ?? Infinity;
        if (possibleDistance < bestDistance) {
          predecessors[neighbour] = current;
          queue.updates(neighbour, () => {
            bestDistances[neighbour] = possibleDistance;
          });
        }
      }
    }
  };

  while (queue.size > 0) {
    const current = queue.pop();
    if (current === undefined) throw new Error();
    const currentDistance = bestDistances[current];
    knownDistances[current] = currentDistance;
    handleChildren(current);
  }

  const target = `${width * 5 - 1},${height * 5 - 1}`;

  console.log({ knownDistances, target });

  return knownDistances[target];
};

const part1 = (input: number[][]): number => {
  console.log({ input });
  return aStar(input);
};

const example = parseInput(`
  1163751742
  1381373672
  2136511328
  3694931569
  7463417111
  1319128137
  1359912421
  3125421639
  1293138521
  2311944581
`);

assertEquals(part1(example), 315);

console.log("Result part 1: " + part1(input));
// 2927 too high
