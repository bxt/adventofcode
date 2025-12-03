// run with `deno run --allow-read=input.txt main.ts`

const file = (await Deno.readTextFile("input.txt")).trim();

const lines = file.split("\n");

let part1 = 0;

for (const line of lines) {
  let largestNumber = -Infinity;
  let afterwardsLargestNumber = -Infinity;

  for (let charIndex = 0; charIndex < line.length; charIndex++) {
    const char = line.charAt(charIndex);
    const number = parseInt(char, 10);

    if (number > largestNumber && charIndex !== line.length - 1) {
      afterwardsLargestNumber = -Infinity;
      largestNumber = number;
    } else if (number > afterwardsLargestNumber) {
      afterwardsLargestNumber = number;
    }
  }

  part1 += largestNumber * 10 + afterwardsLargestNumber;
}

console.log(`Part 1: ${part1}`);
