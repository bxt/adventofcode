#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import { minMax, minusSets, range } from "../utils.ts";

const rangeFromTo = (from: number, to: number): number[] =>
  range(to - from + 1).map((n) => n + from);

assertEquals(rangeFromTo(2, 5), [2, 3, 4, 5]);

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

const [minSeatId, maxSeatId] = minMax(seatIds);

console.log("Result part 1: " + maxSeatId);

const missingSeatIds = minusSets(rangeFromTo(minSeatId, maxSeatId), seatIds);

assertEquals(missingSeatIds.size, 1);

const [missingSeatId] = missingSeatIds;

console.log("Result part 2: " + missingSeatId);
