// run with `deno run --allow-read=input.txt main.ts`
import { assertEquals } from "jsr:@std/assert";

const example = `
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
`;

type ParsedInput = {
  rules: Record<number, Record<number, boolean>>;
  updates: number[][];
};

const parse = (input: string): ParsedInput => {
  const [rulesString, updateString] = input.trim().split("\n\n");

  const rules: ParsedInput["rules"] = {};
  for (const ruleString of rulesString.split("\n")) {
    const [first, second] = ruleString.split("|").map((s) => parseInt(s, 10));
    rules[first] ??= {};
    rules[first][second] = true;
  }

  const updates = updateString
    .split("\n")
    .map((update) => update.split(",").map((s) => parseInt(s, 10)));

  return { rules, updates };
};

const correctlyOrderedUpdates = ({
  rules,
  updates,
}: ParsedInput): ParsedInput["updates"] =>
  updates.filter((update) => {
    for (let i = 0; i < update.length; i++) {
      const first = update[i];
      const forbiddenPrecursors = rules[first];
      if (!forbiddenPrecursors) continue;
      for (let k = 0; k < i; k++) {
        const second = update[k];
        if (forbiddenPrecursors[second]) return false;
      }
    }

    return true;
  });

const middlePageNumber = (update: ParsedInput["updates"][number]): number =>
  update[Math.floor(update.length / 2)];

{
  const parsedInput = parse(example);
  assertEquals(parsedInput["rules"][47][53], true);
  assertEquals(parsedInput["rules"][47][1], undefined);
  assertEquals(parsedInput["updates"].length, 6);
  assertEquals(parsedInput["updates"][0], [75, 47, 61, 53, 29]);
  assertEquals(correctlyOrderedUpdates(parsedInput).length, 3);
  assertEquals(middlePageNumber([75, 47, 61, 53, 29]), 61);
  assertEquals(
    correctlyOrderedUpdates(parsedInput).map(middlePageNumber),
    [61, 53, 29]
  );
}

const file = await Deno.readTextFile("input.txt");

const parsedInput = parse(file);

const part1 = correctlyOrderedUpdates(parsedInput)
  .map(middlePageNumber)
  .reduce((acc, i) => acc + i, 0);

console.log(`Part 1: ${part1}`);
