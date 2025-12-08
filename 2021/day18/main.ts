#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";

type SnailfishNumber = number | [SnailfishNumber, SnailfishNumber];

const parseInput = (string: string): string[] => {
  return string.trim().split("\n").map((line) => line.trim());
};

const explode = (string: string) => {
  let depth = 0;
  let start: number | undefined;
  let end: number | undefined;
  for (let i = 0; i < string.length; i++) {
    if (string.charAt(i) === "[") {
      depth++;
      if (depth === 5) {
        start = i;
      }
    }
    if (string.charAt(i) === "]") {
      if (depth === 5) {
        end = i + 1;
        break;
      }
      depth--;
    }
  }

  if (start === undefined || end === undefined) return string;

  const before = string.substring(0, start);
  const self = string.substring(start, end);
  const after = string.substring(end);

  const [a, b] = JSON.parse(self);

  const beforeNew = before.replace(
    /(\d+)([^0-9]*)$/,
    (_, number, rest) => `${parseInt(number, 10) + a}${rest}`,
  );
  const selfNew = "0";
  const afterNew = after.replace(
    /^([^0-9]*)(\d+)/,
    (_, rest, number) => `${rest}${parseInt(number, 10) + b}`,
  );

  return `${beforeNew}${selfNew}${afterNew}`;
};

assertEquals(explode("[[[[[9,8],1],2],3],4]"), "[[[[0,9],2],3],4]");
assertEquals(explode("[7,[6,[5,[4,[3,2]]]]]"), "[7,[6,[5,[7,0]]]]");
assertEquals(explode("[[6,[5,[4,[3,2]]]],1]"), "[[6,[5,[7,0]]],3]");
assertEquals(
  explode("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"),
  "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]",
);
assertEquals(
  explode("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"),
  "[[3,[2,[8,0]]],[9,[5,[7,0]]]]",
);

assertEquals(
  explode("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"),
  "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]",
);

const split = (string: string) => {
  return string.replace(/\d\d+/, (m) => {
    const half = parseInt(m, 10) / 2;
    return JSON.stringify([Math.floor(half), Math.ceil(half)]);
  });
};
assertEquals(
  split("[[[[0,7],4],[15,[0,13]]],[1,1]]"),
  "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]",
);
assertEquals(
  split("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"),
  "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]",
);

const addAndReduce = (a: string, b: string): string => {
  let current = `[${a},${b}]`;

  const runAndReturnIfChanged = (f: ((s: string) => string)): boolean => {
    const after = f(current);
    if (current === after) return false;
    current = after;
    return true;
  };

  while (runAndReturnIfChanged(explode) || runAndReturnIfChanged(split));

  return current;
};

assertEquals(
  addAndReduce("[[[[4,3],4],4],[7,[[8,4],9]]]", "[1,1]"),
  "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]",
);

const magnitude = (
  n: SnailfishNumber,
): number => {
  if (Array.isArray(n)) {
    const [a, b] = n;
    return 3 * magnitude(a) + 2 * magnitude(b);
  } else {
    return n;
  }
};
assertEquals(magnitude([9, 1]), 29);
assertEquals(magnitude([1, 9]), 21);
assertEquals(magnitude([[9, 1], [1, 9]]), 129);
assertEquals(magnitude([[1, 2], [[3, 4], 5]]), 143);
assertEquals(
  magnitude([[[[0, 7], 4], [[7, 8], [6, 0]]], [8, 1]]),
  1384,
);
assertEquals(
  magnitude([[[[1, 1], [2, 2]], [3, 3]], [4, 4]]),
  445,
);
assertEquals(
  magnitude([[[[3, 0], [5, 3]], [4, 4]], [5, 5]]),
  791,
);
assertEquals(
  magnitude([[[[5, 0], [7, 4]], [5, 5]], [6, 6]]),
  1137,
);
assertEquals(
  magnitude([[[[8, 7], [7, 7]], [[8, 6], [7, 7]]], [[[0, 7], [
    6,
    6,
  ]], [8, 7]]]),
  3488,
);
assertEquals(
  magnitude([[[[6, 6], [7, 6]], [[7, 7], [7, 0]]], [[[7, 7], [7, 7]], [[7, 8], [
    9,
    9,
  ]]]]),
  4140,
);

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

const part1 = (input: string[]): number => {
  return magnitude(JSON.parse(input.reduce(addAndReduce)));
};

const example = parseInput(`
  [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
  [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
  [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
  [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
  [7,[5,[[3,8],[1,4]]]]
  [[2,[2,2]],[8,[8,1]]]
  [2,9]
  [1,[[[9,3],9],[[9,0],[0,7]]]]
  [[[5,[7,4]],7],1]
  [[[[4,2],2],6],[8,7]]
`);

assertEquals(part1(example), 3488);

const example2 = parseInput(`
  [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
  [[[5,[2,8]],4],[5,[[9,9],0]]]
  [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
  [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
  [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
  [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
  [[[[5,4],[7,7]],8],[[8,3],8]]
  [[9,3],[[9,9],[6,[4,9]]]]
  [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
  [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
`);

assertEquals(part1(example2), 4140);

console.log("Result part 1: " + part1(input));

const part2 = (input: string[]): number => {
  let maxMagnitude = -1;

  for (const a of input) {
    for (const b of input) {
      const m = magnitude(JSON.parse(addAndReduce(a, b)));
      if (m > maxMagnitude) maxMagnitude = m;
    }
  }

  return maxMagnitude;
};

assertEquals(part2(example2), 3993);

console.log("Result part 2: " + part2(input));
