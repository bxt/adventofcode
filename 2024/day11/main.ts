// run with `deno run --allow-read=input.txt main.ts`
import { assertEquals } from "jsr:@std/assert";

const input = (await Deno.readTextFile("input.txt")).trim();

const parse = (input: string): number[] => input.split(" ").map(Number);

type Frequencies = Record<string, number>;

const frequencies = (numbers: number[]): Frequencies => {
  const result: Frequencies = {};
  numbers.forEach((number) => {
    result[number] ??= 0;
    result[number]++;
  });
  return result;
};

const sumFrequencies = (numbers: Frequencies): number => {
  return Object.values(numbers).reduce((acc, count) => acc + count, 0);
};

const numbers = frequencies(parse(input));

const blink = (numbers: Frequencies): Frequencies => {
  const result: Frequencies = {};

  for (const [numberString, count] of Object.entries(numbers)) {
    const add = (number: string): void => {
      result[number] ??= 0;
      result[number] += count;
    };

    if (numberString === "0") {
      add("1");
    } else {
      if (numberString.length % 2 === 0) {
        const half = numberString.length / 2;
        add(numberString.slice(0, half));
        add(numberString.slice(half).replace(/^0+(.)/, '$1'));
      } else {
        add((parseInt(numberString, 10) * 2024).toString());
      }
    }
  }

  return result;
};

const blinkTimes = (numbers: Frequencies, times: number): Frequencies => {
  let result = numbers;
  for (let i = 0; i < times; i++) {
    result = blink(result);
  }
  return result;
};

{
  const example1 = frequencies(parse("0 1 10 99 999"));
  const example2 = frequencies(parse("125 17"));

  assertEquals(blink(example1), frequencies([1, 2024, 1, 0, 9, 9, 2021976]));
  assertEquals(blink(example2), frequencies([253000, 1, 7]));
  assertEquals(blinkTimes(example2, 2), frequencies([253, 0, 2024, 14168]));
  assertEquals(blinkTimes(example2, 2), frequencies([253, 0, 2024, 14168]));
  assertEquals(blinkTimes(example2, 3), frequencies([512072, 1, 20, 24, 28676032]));

  assertEquals(
    blinkTimes(example2, 4),
    frequencies([512, 72, 2024, 2, 0, 2, 4, 2867, 6032])
  );

  assertEquals(
    blinkTimes(example2, 5),
    frequencies([1036288, 7, 2, 20, 24, 4048, 1, 4048, 8096, 28, 67, 60, 32])
  );

  assertEquals(
    blinkTimes(example2, 6),
    frequencies([
      2097446912, 14168, 4048, 2, 0, 2, 4, 40, 48, 2024, 40, 48, 80, 96, 2, 8,
      6, 7, 6, 0, 3, 2,
    ])
  );
  assertEquals(sumFrequencies(blinkTimes(example2, 25)), 55312);
}

console.log(`Part 1: ${sumFrequencies(blinkTimes(numbers, 25))}`);
console.log(`Part 2: ${sumFrequencies(blinkTimes(numbers, 75))}`);
