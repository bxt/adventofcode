#!/usr/bin/env deno test --allow-read --coverage --unstable
import {
  assertEquals,
  assertThrows,
} from "https://deno.land/std@0.79.0/testing/asserts.ts";
import {
  addCoords,
  ensureElementOf,
  manhattanNormCoord,
  matchGroups,
  maybeElementOf,
  product,
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
