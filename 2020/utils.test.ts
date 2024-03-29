#!/usr/bin/env deno test --allow-read

/*
 * To check coverage, run: deno test --allow-read --coverage=coverage --unstable utils.test.ts; deno coverage coverage; rm -R coverage/
 */

import {
  assertEquals,
  assertThrows,
} from "https://deno.land/std@0.116.0/testing/asserts.ts";
import {
  addCoords,
  boundsOfCoords,
  Coord,
  CoordSet,
  ensureElementOf,
  indexWithCoord,
  intersectSets,
  manhattanNormCoord,
  matchGroups,
  maybeElementOf,
  minMax,
  minusSets,
  product,
  range,
  rangeCoords,
  rotateLeftNinetyDegreesCoord,
  scaleCoord,
  sum,
} from "./utils.ts";

Deno.test("sum", () => {
  assertEquals(sum([1, 4, 6, 4]), 15);
  assertEquals(sum([1, 4, -6, 4]), 3);
});

Deno.test("product", () => {
  assertEquals(product([1, 4, 6, 4]), 96);
  assertEquals(product([1, 4, -6, 4]), -96);
  assertEquals(product([1, 4, 0, 6]), 0);
});

Deno.test("minMax", () => {
  assertEquals(minMax([]), [Infinity, -Infinity]);
  assertEquals(minMax([1, 4, 6, 4]), [1, 6]);
  assertEquals(minMax([1, 4, -6, 4]), [-6, 4]);
  assertEquals(minMax(new Set([1, 4, 6])), [1, 6]);
});

Deno.test("range", () => {
  assertEquals(range(3), [0, 1, 2]);
  assertEquals(range(0), []);
  assertThrows((): void => {
    range(-1);
  });
});

Deno.test("addCoords", () => {
  assertEquals(addCoords([7, 13], [42, 9]), [49, 22]);
  assertEquals(addCoords([-7, 13], [42, 0]), [35, 13]);
});

Deno.test("scaleCoord", () => {
  assertEquals(scaleCoord([7, 13], 5), [35, 65]);
  assertEquals(scaleCoord([-7, 13], -2), [14, -26]);
});

Deno.test("manhattanNormCoord", () => {
  assertEquals(manhattanNormCoord([7, 13]), 20);
  assertEquals(manhattanNormCoord([-7, 13]), 20);
});

Deno.test("rotateLeftNinetyDegreesCoord", () => {
  assertEquals(rotateLeftNinetyDegreesCoord([7, 13]), [-13, 7]);
  assertEquals(rotateLeftNinetyDegreesCoord([-7, 13]), [-13, -7]);
  assertEquals(rotateLeftNinetyDegreesCoord([0, 13]), [-13, 0]);
});

Deno.test("indexWithCoord", () => {
  assertEquals(indexWithCoord([["a", "b"], ["c", "d"]], [0, 0]), "a");
  assertEquals(indexWithCoord([["a", "b"], ["c", "d"]], [1, 0]), "b");
  assertEquals(indexWithCoord([["a", "b"], ["c", "d"]], [0, 1]), "c");
  assertEquals(indexWithCoord([["a", "b"], ["c", "d"]], [1, 1]), "d");
  assertEquals(indexWithCoord({ 3: { 6: "Z" } }, [6, 3]), "Z");
});

Deno.test("boundsOfCoords", () => {
  assertEquals(boundsOfCoords([[0, 0], [0, 0], [0, 0]]), [[0, 0], [0, 0]]);
  assertEquals(
    boundsOfCoords([[1, 10], [7, 0], [-3, -1]]),
    [[-3, -1], [7, 10]],
  );
});

Deno.test("rangeCoords", () => {
  assertEquals(rangeCoords([0, 0]), []);
  assertEquals(
    rangeCoords([3, 2]),
    [[0, 0], [1, 0], [2, 0], [0, 1], [1, 1], [2, 1]],
  );
});

Deno.test("maybeElementOf", () => {
  assertEquals(maybeElementOf(7, [7, 13]), 7);
  assertEquals(maybeElementOf(6, [7, 13]), null);
  // @ts-expect-error We should be notified that this check does not make sense
  assertEquals(maybeElementOf("oy", [7, 13]), null);
  // @ts-expect-error We should be notified that this check does not make sense
  assertEquals(maybeElementOf(4, ["a", "b"]), null);
});

Deno.test("ensureElementOf", () => {
  assertEquals(ensureElementOf(7, [7, 13]), 7);
  assertThrows((): void => {
    ensureElementOf(6, [7, 13]);
  });
  assertThrows((): void => {
    // @ts-expect-error We should be notified that this check does not make sense
    ensureElementOf("oy", [7, 13]);
  });
  assertThrows((): void => {
    // @ts-expect-error We should be notified that this check does not make sense
    ensureElementOf(4, ["a", "b"]);
  });
});

Deno.test("matchGroups", () => {
  assertThrows((): void => {
    assertEquals(matchGroups(/a/)("a"), {});
  });
  assertThrows((): void => {
    assertEquals(matchGroups(/a/)("b"), {});
  });
  assertEquals(matchGroups(/a(?<b>c)/)("ac"), { b: "c" });
  assertEquals(
    matchGroups(/a(?<b>\d+)c(?<d>e?)/)("a123c"),
    { b: "123", d: "" },
  );
});

Deno.test("intersectSets", () => {
  assertEquals(
    intersectSets(
      new Set([1, 2, 3, 4]),
      new Set([1, 2, 3]),
      new Set([2, 3, 4]),
    ),
    new Set([2, 3]),
  );
  assertEquals(intersectSets(new Set([]), new Set([1, 2, 3])), new Set());
  assertEquals(intersectSets(new Set([1, 2, 3]), new Set([])), new Set());
  assertEquals(
    intersectSets([1, 2, 3, 4], new Set([1, 2, 3]), new Set([2, 3, 4])),
    new Set([2, 3]),
  );
});

Deno.test("minusSets", () => {
  assertEquals(minusSets(new Set([]), new Set([1, 2, 3])), new Set());
  assertEquals(minusSets(new Set([1, 2, 3]), new Set([])), new Set([1, 2, 3]));
  assertEquals(
    minusSets(
      new Set([1, 2, 3, 4, 5]),
      new Set([1, 2, 6, 7, 8]),
    ),
    new Set([3, 4, 5]),
  );
  assertEquals(minusSets([1, 2, 3, 4, 5], new Set([1, 2, 3])), new Set([4, 5]));
});

Deno.test("CoordSet (and StringifySinkSet, StringifySet)", () => {
  const coordSet = new CoordSet();
  assertEquals(coordSet.size, 0);

  coordSet.add([5, -3]);
  coordSet.add([10, 11]);
  coordSet.add([0, 0]);
  assertEquals(coordSet.size, 3);
  assertEquals(coordSet.has([5, -3]), true);
  assertEquals(coordSet.has([10, 11]), true);
  assertEquals(coordSet.has([0, 0]), true);
  assertEquals(coordSet.has([42, 6]), false);

  assertEquals([...coordSet.keys()], [[5, -3], [10, 11], [0, 0]]);
  assertEquals([...coordSet.values()], [[5, -3], [10, 11], [0, 0]]);
  assertEquals(
    [...coordSet.entries()],
    [[[5, -3], [5, -3]], [[10, 11], [10, 11]], [[0, 0], [0, 0]]],
  );

  coordSet.add([10, 11]);
  assertEquals(coordSet.size, 3);
  assertEquals(coordSet.has([10, 11]), true);

  coordSet.delete([10, 11]);
  assertEquals(coordSet.size, 2);
  assertEquals(coordSet.has([10, 11]), false);

  const forEachCalls: [Coord, Coord, Set<Coord>][] = [];
  coordSet.forEach((...args) => forEachCalls.push(args));
  assertEquals(forEachCalls, [
    [[5, -3], [5, -3], coordSet],
    [[0, 0], [0, 0], coordSet],
  ]);

  assertEquals(coordSet.toString(), "[object StringifySet]");

  coordSet.clear();
  assertEquals(coordSet.size, 0);
  assertEquals(coordSet.has([5, -3]), false);
  assertEquals(coordSet.has([0, 0]), false);

  const coordSet2 = new CoordSet([[6, 7]]);
  assertEquals(coordSet2.size, 1);
  assertEquals(coordSet2.has([6, 7]), true);

  const aBunchOfCooords = rangeCoords([11, 11]).map((c) =>
    addCoords(c, [-5, -5])
  );
  assertEquals(aBunchOfCooords, [...new CoordSet(aBunchOfCooords).values()]);
});
