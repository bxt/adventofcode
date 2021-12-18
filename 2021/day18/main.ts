#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";

type SnailfishNumber = number | [SnailfishNumber, SnailfishNumber];

const parseInput = (string: string): SnailfishNumber[] => {
  return string.trim().split("\n").map((line) => JSON.parse(line.trim()));
};

const addSnailfishNumbers = (
  a: SnailfishNumber,
  b: SnailfishNumber,
): SnailfishNumber => [a, b];

const explode = (n: SnailfishNumber) => {
  const string = JSON.stringify(n);
  let depth = 0;
  let maxDepth = 0;
  let start: number | undefined;
  let end: number | undefined;
  for (let i = 0; i < string.length; i++) {
    if (string.charAt(i) === "[") {
      depth++;
      if (depth > maxDepth && depth >= 5) {
        if (depth >= 6) throw new Error("Nested too deep");
        start = i;
        end = undefined;
        maxDepth = depth;
      }
    }
    if (string.charAt(i) === "]") {
      if (depth === maxDepth && end === undefined) {
        end = i + 1;
        break;
      }
      depth--;
    }
  }
  if (start !== undefined && end !== undefined) {
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

    const resultString = `${beforeNew}${selfNew}${afterNew}`;
    console.log({
      string,
      start,
      end,
      before,
      self,
      after,
      a,
      b,
      beforeNew,
      selfNew,
      afterNew,
      resultString,
    });
    return JSON.parse(resultString);
  }

  return n;
};

assertEquals(explode([[[[[9, 8], 1], 2], 3], 4]), [[[[0, 9], 2], 3], 4]);
assertEquals(explode([7, [6, [5, [4, [3, 2]]]]]), [7, [6, [5, [7, 0]]]]);
assertEquals(explode([[6, [5, [4, [3, 2]]]], 1]), [[6, [5, [7, 0]]], 3]);
assertEquals(explode([[3, [2, [1, [7, 3]]]], [6, [5, [4, [3, 2]]]]]), [[3, [2, [
  8,
  0,
]]], [9, [5, [4, [3, 2]]]]]);
assertEquals(explode([[3, [2, [8, 0]]], [9, [5, [4, [3, 2]]]]]), [[3, [2, [
  8,
  0,
]]], [9, [5, [7, 0]]]]);

assertEquals(explode([[[[[4, 3], 4], 4], [7, [[8, 4], 9]]], [1, 1]]), [[[
  [0, 7],
  4,
], [7, [[8, 4], 9]]], [1, 1]]);

const split = (n: SnailfishNumber) => {
  return JSON.parse(
    JSON.stringify(n).replace(/\d\d+/, (m) => {
      const half = parseInt(m, 10) / 2;
      return JSON.stringify([Math.floor(half), Math.ceil(half)]);
    }),
  );
};
assertEquals(split([[[[0, 7], 4], [15, [0, 13]]], [1, 1]]), [[[[0, 7], 4], [[
  7,
  8,
], [0, 13]]], [1, 1]]);
assertEquals(split([[[[0, 7], 4], [[7, 8], [0, 13]]], [1, 1]]), [[[[0, 7], 4], [
  [7, 8],
  [0, [6, 7]],
]], [1, 1]]);

const addAndReduce = (
  a: SnailfishNumber,
  b: SnailfishNumber,
): SnailfishNumber => {
  let current = addSnailfishNumbers(a, b);
  while (true) {
    const exploded = explode(current);
    if (JSON.stringify(exploded) === JSON.stringify(current)) {
      const splitted = split(exploded);

      if (JSON.stringify(splitted) === JSON.stringify(current)) {
        break;
      } else {
        current = splitted;
      }
    } else {
      current = exploded;
    }
  }
  console.log("#############");
  console.log(JSON.stringify(current));
  console.log("#############");
  return current;
};

assertEquals(addAndReduce([[[[4, 3], 4], 4], [7, [[8, 4], 9]]], [1, 1]), [[[[
  0,
  7,
], 4], [[7, 8], [6, 0]]], [8, 1]]);

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

const part1 = (input: SnailfishNumber[]): number => {
  return magnitude(input.reduce(addAndReduce));
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
