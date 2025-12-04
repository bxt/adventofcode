// run with `deno run --allow-read=input.txt vis.ts`

import { field, findRemovals } from "./main.ts";

let part1: undefined | number = undefined;
let part2: number = 0;

const offsetMap = [0, 1, 2, 6, 3, 4, 5, 7];

const magentize = (text: string) => `\x1b[35m${text}\x1b[0m`;

while (true) {
  console.clear();

  const toRemove = findRemovals();
  const toRemoveSet = new Set(
    toRemove.map(([lineIndex, charIndex]) => `${lineIndex},${charIndex}`),
  );

  const addition = toRemove.length ? magentize(` (-${toRemove.length})`) : "";
  console.log(`Part 1: ${part1 ?? "????"} – Part 2: ${part2}${addition}\n`);

  for (let lineIndex = 0; lineIndex < field.length; lineIndex += 4) {
    const line = field[lineIndex];

    const lineStrings = [];

    for (let charIndex = 0; charIndex < line.length; charIndex += 2) {
      let offset = 0;
      let isRemoved = false;

      for (const subLine of [0, 1, 2, 3] as const) {
        for (const subChar of [0, 1] as const) {
          const currentLine = lineIndex + subLine;
          const currentChar = charIndex + subChar;
          if (field?.[currentLine]?.[currentChar]) {
            offset += 1 << offsetMap[subLine + 4 * subChar];
            isRemoved ||= toRemoveSet.has(`${currentLine},${currentChar}`);
          }
        }
      }

      const brailleBlock = String.fromCodePoint(0x2800 + offset);
      lineStrings.push(isRemoved ? magentize(brailleBlock) : brailleBlock);
    }

    console.log(lineStrings.join(""));
  }

  for (const [lineIndex, charIndex] of toRemove) {
    field[lineIndex][charIndex] = false;
  }

  if (part1 === undefined) part1 = toRemove.length;
  part2 += toRemove.length;
  if (toRemove.length === 0) break;
  const sleepTime = Math.log(toRemove.length) * 20 + 30;
  await new Promise((resolve) => setTimeout(resolve, sleepTime));
}
