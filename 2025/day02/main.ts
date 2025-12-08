#!/usr/bin/env deno run --allow-read=input.txt

const file = await Deno.readTextFile("input.txt");

const regex = /(\d+)-(\d+)/;

const ranges = file.split(",").map((entry) => {
  const match = regex.exec(entry);
  if (!match) throw new Error(`Invalid entry: ${entry}`);
  const [_, from, to] = match;

  return [parseInt(from, 10), parseInt(to, 10)] as const;
});

let part1 = 0;
let part2 = 0;

for (const [from, to] of ranges) {
  withNextId: for (let id = from; id <= to; id++) {
    const idString = id.toString();

    // This also works but is around half as fast:
    // if (/^(.+)\1$/.test(idString)) part1 += id;
    // if (/^(.+)\1+$/.test(idString)) part2 += id;

    withNextPieceCount: for (
      let pieces = 2;
      pieces <= idString.length;
      pieces++
    ) {
      if (idString.length % pieces !== 0) continue withNextPieceCount;
      const pieceLength = idString.length / pieces;

      const reference = idString.slice(0, pieceLength);

      for (let piece = 1; piece < pieces; piece++) {
        const pieceString = idString.slice(
          pieceLength * piece,
          pieceLength * (piece + 1),
        );
        if (pieceString !== reference) continue withNextPieceCount;
      }

      if (pieces === 2) {
        part1 += id;
      }
      part2 += id;
      continue withNextId;
    }
  }
}

console.log(`Part 1: ${part1}`);
console.log(`Part 2: ${part2}`);
