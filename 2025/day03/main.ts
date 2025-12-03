// run with `deno run --allow-read=input.txt main.ts`

const file = (await Deno.readTextFile("input.txt")).trim();

const lines = file.split("\n");

const sum = (numbers: number[]): number => {
  return numbers.reduce((acc, number) => acc + number, 0);
};

const getJoltage = (digits: number) => (line: string): number => {
  const selectedDigits = new Array(digits).fill(-Infinity);

  for (let charIndex = 0; charIndex < line.length; charIndex++) {
    const char = line.charAt(charIndex);
    const number = parseInt(char, 10);
    const leftoverChars = line.length - charIndex;

    for (let i = 0; i < selectedDigits.length; i++) {
      const isImprovement = number > selectedDigits[i];
      const enoughRemaining = (selectedDigits.length - i) <= leftoverChars;

      if (isImprovement && enoughRemaining) {
        selectedDigits[i] = number;

        for (let k = i + 1; k < selectedDigits.length; k++) {
          selectedDigits[k] = -Infinity;
        }

        break;
      }
    }
  }

  return parseInt(selectedDigits.join(""), 10);
};

console.log(`Part 1: ${sum(lines.map(getJoltage(2)))}`);
console.log(`Part 2: ${sum(lines.map(getJoltage(12)))}`);
