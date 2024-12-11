// run with `deno run --allow-read=input.txt main.ts`
import { assertEquals } from "jsr:@std/assert";

const input = (await Deno.readTextFile("input.txt")).trim();

const parse = (input: string): number[] => input.split(" ").map(Number);

const numbers = parse(input);

const blink = (numbers: number[]): number[] => {
  return numbers.flatMap((number) => {
    if (number === 0) return [1];

    const numberString = number.toString();
    if (numberString.length % 2 === 0) {
      const half = numberString.length / 2;
      return [
        parseInt(numberString.slice(0, half), 10),
        parseInt(numberString.slice(half), 10),
      ];
    }

    return [number * 2024];
  });
};

const blinkTimes = (numbers: number[], times: number): number[] => {
  let result = numbers;
  for (let i = 0; i < times; i++) {
    result = blink(result);
  }
  return result;
};

{
  const example1 = parse("0 1 10 99 999");
  const example2 = parse("125 17");

  assertEquals(blink(example1), [1, 2024, 1, 0, 9, 9, 2021976]);
  assertEquals(blink(example2), [253000, 1, 7]);
  assertEquals(blinkTimes(example2, 2), [253, 0, 2024, 14168]);
  assertEquals(blinkTimes(example2, 2), [253, 0, 2024, 14168]);
  assertEquals(blinkTimes(example2, 3), [512072, 1, 20, 24, 28676032]);

  assertEquals(
    blinkTimes(example2, 4),
    [512, 72, 2024, 2, 0, 2, 4, 2867, 6032]
  );

  assertEquals(
    blinkTimes(example2, 5),
    [1036288, 7, 2, 20, 24, 4048, 1, 4048, 8096, 28, 67, 60, 32]
  );

  assertEquals(
    blinkTimes(example2, 6),
    [
      2097446912, 14168, 4048, 2, 0, 2, 4, 40, 48, 2024, 40, 48, 80, 96, 2, 8,
      6, 7, 6, 0, 3, 2,
    ]
  );
  assertEquals(blinkTimes(example2, 25).length, 55312);
}

console.log(`Part 1: ${blinkTimes(numbers, 25).length}`);
