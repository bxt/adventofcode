#!/usr/bin/env deno test --allow-read
import {
  assertEquals,
  assertThrows,
} from "https://deno.land/std@0.79.0/testing/asserts.ts";
import {
  addCoords,
  ensureElementOf,
  manhattanNormCoord,
  rotateLeftNinetyDegreesCoord,
  scaleCoord,
  sum,
} from "./utils.ts";

Deno.test("sum", () => {
  assertEquals(sum([1, 4, 6, 4]), 15);
  assertEquals(sum([1, 4, -6, 4]), 3);
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
  assertEquals(rotateLeftNinetyDegreesCoord([7, 13]), [13, -7]);
  assertEquals(rotateLeftNinetyDegreesCoord([-7, 13]), [13, 7]);
  assertEquals(rotateLeftNinetyDegreesCoord([0, 13]), [13, 0]);
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
