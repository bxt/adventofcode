// run with `deno run --allow-read=input.txt main.ts`

const file = await Deno.readTextFile("input.txt");

const [rangesString, idsString] = file.split("\n\n");

const regex = /(\d+)-(\d+)/;

const ranges = rangesString.trim().split("\n").map((entry) => {
  const match = regex.exec(entry);
  if (!match) throw new Error(`Invalid entry: "${entry}"`);
  const [_, from, to] = match;

  return [parseInt(from, 10), parseInt(to, 10)] as const;
});

const ids = idsString.trim().split("\n").map((entry) => parseInt(entry, 10));

const part1 =
  ids.filter((id) => ranges.some(([from, to]) => id >= from && id <= to))
    .length;

console.log(`Part 1: ${part1}`);

const sortedRanges = ranges.slice().sort((a, b) => a[0] - b[0]);

let part2 = 0;
let processedUntil = -Infinity;

for (const [from, to] of sortedRanges) {
  if (to <= processedUntil) continue;
  part2 += to - Math.max(from - 1, processedUntil);
  processedUntil = to;
}

console.log(`Part 2: ${part2}`);
