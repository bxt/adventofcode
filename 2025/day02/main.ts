// run with `deno run --allow-read=input.txt main.ts`

const file = await Deno.readTextFile("input.txt");

const regex = /(\d+)-(\d+)/;

const ranges = file.split(",").map((entry) => {
  const match = regex.exec(entry);
  if (!match) throw new Error(`Invalid entry: ${entry}`);
  const [_, from, to] = match;

  return [parseInt(from, 10), parseInt(to, 10)] as const;
});

let part1 = 0;

for (const [from, to] of ranges) {
  for (let id = from; id <= to; id++) {
    const idString = id.toString();
    if (idString.length % 2 !== 0) continue;

    const mid = idString.length / 2;
    const firstHalf = idString.slice(0, mid);
    const secondHalf = idString.slice(mid);
    if (firstHalf === secondHalf) {
      part1 += id;
    }
  }
}

console.log(`Part 1: ${part1}`);
