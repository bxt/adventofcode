// run with `deno run --allow-read=input.txt main.ts`
import { assertEquals } from "jsr:@std/assert";

const input = (await Deno.readTextFile("input.txt")).trim();

const parse = (input: string): number[] => input.split("").map(Number);

const numbers = parse(input);

const calculateChecksum = (numbers: number[]): number => {
  let result = 0;
  let diskPointer = 0;

  let lastNumberIndex = numbers.length + 1;
  let leftoverLast = 0;

  for (let i = 0; i < lastNumberIndex; i++) {
    if (i % 2 === 0) {
      // file
      const fileId = i / 2;
      for (let j = 0; j < numbers[i]; j++) {
        result += diskPointer * fileId;
        console.log(`${diskPointer} * ${fileId}`);
        diskPointer++;
      }
    } else {
      // free space
      for (let j = 0; j < numbers[i]; j++) {
        if (leftoverLast === 0) {
          lastNumberIndex -= 2;
          leftoverLast = numbers[lastNumberIndex];
        }
        leftoverLast--;
        const fileId = lastNumberIndex / 2;
        result += diskPointer * fileId;
        console.log(`${diskPointer} * ${fileId}`);
        diskPointer++;
      }
    }
  }

  while (leftoverLast !== 0) {
    leftoverLast--;
    const fileId = lastNumberIndex / 2;
    result += diskPointer * fileId;
    console.log(`${diskPointer} * ${fileId}`);
    diskPointer++;
  }

  return result;
};

{
  const example = "2333133121414131402";
  assertEquals(calculateChecksum(parse(example)), 1928);
}

console.log(`Part 1: ${calculateChecksum(numbers)}`);
