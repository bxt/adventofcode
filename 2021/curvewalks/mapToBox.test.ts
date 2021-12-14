#!/usr/bin/env deno test --allow-read

import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { mapToBox } from "./mapToBox.ts";

Deno.test("mapToBox", () => {
  assertEquals(mapToBox([[0, 0], [1, 1]], 10, 120, 120), [[10, 10], [
    110,
    110,
  ]]);
  assertEquals(mapToBox([[0, 0], [2, 1]], 10, 220, 120), [[10, 10], [
    210,
    110,
  ]]);
  assertEquals(mapToBox([[0, 0], [2, 1]], 10, 120, 120), [[10, 35], [110, 85]]);
  assertEquals(mapToBox([[0, 0], [1, 2]], 10, 120, 120), [[35, 10], [85, 110]]);

  assertEquals(mapToBox([[3, 5], [4, 6]], 10, 120, 120), [[10, 10], [
    110,
    110,
  ]]);
  assertEquals(mapToBox([[3, 5], [5, 6]], 10, 220, 120), [[10, 10], [
    210,
    110,
  ]]);
  assertEquals(mapToBox([[3, 5], [5, 6]], 10, 120, 120), [[10, 35], [110, 85]]);
  assertEquals(mapToBox([[3, 5], [4, 7]], 10, 120, 120), [[35, 10], [85, 110]]);

  assertEquals(mapToBox([[-3, -1], [-2, 0]], 4, 40, 40), [[4, 4], [36, 36]]);
  assertEquals(mapToBox([[-3, -1], [-1, 0]], 4, 72, 40), [[4, 4], [68, 36]]);
  assertEquals(mapToBox([[-3, -1], [-1, 0]], 4, 40, 40), [[4, 12], [36, 28]]);
  assertEquals(mapToBox([[-3, -1], [-2, 1]], 4, 40, 40), [[12, 4], [28, 36]]);
});
