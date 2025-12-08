#!/usr/bin/env deno run --allow-read=input.txt
import { assertEquals } from "jsr:@std/assert";

const input = (await Deno.readTextFile("input.txt")).trim();

const parse = (input: string): number[] => input.split("").map(Number);

const numbers = parse(input);

const example = parse("2333133121414131402");

const calculateChecksum = (numbers: number[]): number => {
  let result = 0;
  let diskPointer = 0;

  let lastNumberIndex = numbers.length + 1;
  let leftoverLast = 0;

  const addToResult = (fileId: number, amount: number): void => {
    result += amount * fileId * (diskPointer + (amount - 1) / 2);
    diskPointer += amount;
  };

  for (let i = 0; i < lastNumberIndex; i++) {
    let size = numbers[i];
    if (i % 2 === 0) {
      addToResult(i / 2, size);
    } else {
      while (size > 0) {
        if (leftoverLast === 0) {
          lastNumberIndex -= 2;
          leftoverLast = numbers[lastNumberIndex];
        }
        const nip = Math.min(size, leftoverLast);
        leftoverLast -= nip;
        size -= nip;
        addToResult(lastNumberIndex / 2, nip);
      }
    }
  }

  if (leftoverLast !== 0) {
    addToResult(lastNumberIndex / 2, leftoverLast);
  }

  return result;
};

assertEquals(calculateChecksum(example), 1928);

console.log(`Part 1: ${calculateChecksum(numbers)}`);

const calculateChecksum2 = (numbers: number[]): number => {
  const disk = numbers.map((size, index) => {
    if (index % 2 === 0) {
      return { kind: "file" as const, size, id: index / 2 };
    } else {
      return { kind: "free" as const, size };
    }
  });

  for (let fileId = Math.floor(numbers.length / 2); fileId > 0; fileId--) {
    const fileIndex = disk.findIndex(
      (file) => file.kind === "file" && file.id === fileId
    );
    const file = disk[fileIndex];
    const { size } = file;
    const moveTo = disk.findIndex(
      ({ kind, size: freeSize }) => kind === "free" && freeSize >= size
    );
    if (moveTo === -1) continue;
    if (moveTo >= fileIndex) continue;
    const { size: freeSize } = disk[moveTo];
    const leftoverFreeSpace = freeSize - size;
    disk.splice(fileIndex, 1, { kind: "free", size });
    if (leftoverFreeSpace > 0) {
      disk.splice(moveTo, 1, file, { kind: "free", size: leftoverFreeSpace });
    } else {
      disk.splice(moveTo, 1, file);
    }
  }

  let result = 0;
  let diskPointer = 0;

  for (const file of disk) {
    if (file.kind === "free") {
      diskPointer += file.size;
    } else {
      for (let i = 0; i < file.size; i++) {
        result += diskPointer * file.id;
        diskPointer++;
      }
    }
  }

  return result;
};

assertEquals(calculateChecksum2(example), 2858);

console.log(`Part 2: ${calculateChecksum2(numbers)}`);
