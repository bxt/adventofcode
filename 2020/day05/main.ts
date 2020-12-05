#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";

const range = (from: number, to: number): number[] =>
  Array(to - from + 1).fill(true).map((_, i) => i + from);

assertEquals(range(2, 5), [2, 3, 4, 5]);

const getSeatId = (seat: string): number =>
  parseInt(
    seat.replaceAll("F", "0").replaceAll("B", "1").replaceAll("L", "0")
      .replaceAll("R", "1"),
    2,
  );

assertEquals(getSeatId("FBFBBFFRLR"), 357);
assertEquals(getSeatId("BFFFBBFRRR"), 567);
assertEquals(getSeatId("FFFBBBFRRR"), 119);
assertEquals(getSeatId("BBFFBBFRLL"), 820);

const input = await Deno.readTextFile("input.txt");

const seatIds = new Set(input.trim().split("\n").map(getSeatId));

const minSeatId = Math.min(...seatIds);
const maxSeatId = Math.max(...seatIds);

console.log("Result part 1: " + maxSeatId);

const missingSeatIds = range(minSeatId, maxSeatId).filter((seatId) =>
  !seatIds.has(seatId)
);

assertEquals(missingSeatIds.length, 1);

console.log("Result part 1: " + missingSeatIds[0]);
