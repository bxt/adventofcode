
const allAligned = (numbers: number[]): boolean => {
  for (let i = 1; i < numbers.length; i++) {
    if (numbers[i] !== numbers[i - 1] + 1) return false;
  }
  return true;
};

assertEquals(allAligned([1, 2, 3]), true);
assertEquals(allAligned([3, 3, 3]), false);

const lcm = (numbers: number[]) => {
  const candidates = [...numbers];
  while (true) {
    const [least, leastIndex] = candidates.reduce(
      ([a, ai], b, bi) => a + ai < b + bi ? [a, ai] : [b, bi],
      [NaN, NaN],
    );
    console.log({ least, leastIndex });
    candidates[leastIndex] += numbers[leastIndex];
    console.log({ candidates });
    if (allAligned(candidates)) return candidates[0];
  }
};

a^x - b^y = 1

assertEquals(lcm([3, 4, 6]), 12);

const part2 = (config: Config): number => {
  const { busLines } = config;
  const adjustedBusLines = busLines.map((b, i) => b === null ? null : b - i);
  const adjustedFilteredsBusLines =
    (adjustedBusLines.filter((b) => b) as number[]);
  console.log({ adjustedFilteredsBusLines });

  return lcm(adjustedFilteredsBusLines);
};

assertEquals(part2(example), 1068788);

console.log("Result part 2: " + part2(inputParsed));
