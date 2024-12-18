import process from "node:process";

let input = 0b011100;

const needle = [/*2,4,1,5,7,5,1,6,0,3,4,6,5,5,*/3,0];

  let needleIndex = 0;

  while(input !== 0) {
    const b = input & 0b111;
    const b1 = b ^ 0b101;
    const c = input >> b1;
    const b2 = (b ^ 0b011 ^ c) & 0b111; // 0b101 ^0b110 = 0b011
    input >>= 3;
    if (needle[needleIndex++] !== b2) process.exit(1);
    if (needleIndex > needle.length) process.exit(1);
    // jnz
  }

  if (needleIndex !== needle.length) process.exit(1);

