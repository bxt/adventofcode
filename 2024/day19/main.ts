const file = await Deno.readTextFile("input.txt");

const [patternsString, designsString] = file.split("\n\n");

const patterns = patternsString.split(", ");

const designs = designsString.split("\n").filter((design) => design);

const possibilities: Record<string, number> = { "": 1 };

const getWays = (design: string): number => {
  if (possibilities[design] !== undefined) return possibilities[design];

  let ways = 0;

  for (const pattern of patterns) {
    if (design.startsWith(pattern)) {
      ways += getWays(design.slice(pattern.length));
    }
  }

  possibilities[design] = ways;

  return ways;
};

let countPossible = 0;
let countWays = 0;

for (const design of designs) {
  const ways = getWays(design);
  countWays += ways;
  if (ways > 0) countPossible++;
}

console.log(`Part 1: ${countPossible}`);
console.log(`Part 2: ${countWays}`);
