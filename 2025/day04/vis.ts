// run with `deno run --allow-read=input.txt vis.ts`

import { field, findRemovals } from "./main.ts";

let part1: undefined | number = undefined;
let part2: number = 0;

while (true) {
  console.clear();
  console.log(`Part 1: ${part1 ?? '????'} – Part 2: ${part2}\n`);

  const toRemove = findRemovals();

    for (let lineIndex = 0; lineIndex < field.length; lineIndex++) {
      const line = field[lineIndex];

      for (let charIndex = 0; charIndex < line.length; charIndex++) {
        if (line[charIndex]) {
          if (toRemove.some(([l, c]) => l === lineIndex && c === charIndex)) {
            Deno.stdout.writeSync(new TextEncoder().encode("x"));
          } else {
            Deno.stdout.writeSync(new TextEncoder().encode("@"));
          }
        } else {
          Deno.stdout.writeSync(new TextEncoder().encode("."));
        }
      }
    }


  for (const [lineIndex, charIndex] of toRemove) {
    field[lineIndex][charIndex] = false;
  }

  if (part1 === undefined) part1 = toRemove.length;
  part2 += toRemove.length;
  if (toRemove.length === 0) break;
  await new Promise((resolve) => setTimeout(resolve, 200));
}

