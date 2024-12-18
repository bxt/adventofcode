

const needle = [2,4,1,5,7,5,1,6,0,3,4,6,5,5,3,0];

function searchAnswer(needle: number[], input: bigint): bigint | undefined {

    console.log(`at ${needle.length}...`);
    input <<= BigInt(3);
    const current = needle[needle.length - 1];
    const candidates = [];

    for (let candidate = BigInt(0); candidate < 8; candidate++) {
      const b = candidate;
      const newInput = input | b;
      const b1 = b ^ BigInt(0b101);
      const c = newInput >> b1;
      const b2 = (b ^ BigInt(0b101^0b110) ^ c) & BigInt(0b111);
      if (b2 === BigInt(current)) {
        candidates.push(b);

      }
    }
    console.log(`candidates.length: ${candidates.length}`);
    if (candidates.length === 0) return undefined;

    for (const candidate of candidates) {
      const newInput = input | candidate;
      if (needle.length === 1) return newInput;
      const result = searchAnswer(needle.slice(0, -1), newInput);
      if (result !== undefined) return result;
    }
}

const input = searchAnswer(needle, BigInt(0));
if (input === undefined) throw new Error("no match \uD83D\uDE2D");

console.log(input);
console.log(input.toString(2));

// import process from "node:process";

  // let needleIndex = 0;

  // while(input !== 0) {
  //   const b = input & 0b111;
  //   const b1 = b ^ 0b101;
  //   const c = input >> b1;
  //   const b2 = (b ^ 0b011 ^ c) & 0b111; // 0b101 ^0b110 = 0b011
  //   input >>= 3;
  //   if (needle[needleIndex++] !== b2) process.exit(1);
  //   if (needleIndex > needle.length) process.exit(1);
  //   // jnz
  // }

  // if (needleIndex !== needle.length) process.exit(1);

