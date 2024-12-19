import { assertEquals } from "jsr:@std/assert";

const file = await Deno.readTextFile("input.txt");

const [patternsString, designsString] = file.split("\n\n");

const patterns = patternsString.split(", ");

const designs = designsString.split("\n");

interface PatternTrie {
  [key: string]: PatternTrie | undefined;
}

const END: PatternTrie = {};

const makeTrie = (patterns: string[]): PatternTrie => {
  const trie: PatternTrie = {};

  for (const pattern of patterns) {
    let current = trie;

    for (const char of pattern) {
      current[char] ??= {};
      current = current[char];
    }

    current["."] = END;
  }

  return trie;
};

const isPossible = (design: string, trie: PatternTrie): boolean => {
  return isPossibleInner(design, 0, trie, trie);
};

const isPossibleInner = (
  design: string,
  at: number,
  trie: PatternTrie,
  fullTrie: PatternTrie
): boolean => {
  // console.log(" ".repeat(at) + "?");
  // console.log(design, at, trie);
  if (at === design.length) return trie["."] !== undefined;

  const char = design[at];

  const subTrie = trie[char];

  if (subTrie === undefined) return false;

  const isDirectlyPossible = isPossibleInner(design, at + 1, subTrie, fullTrie);

  if (isDirectlyPossible) return true;

  const canEndHere = subTrie["."] !== undefined;

  if (!canEndHere) return false;

  return isPossibleInner(design, at + 1, fullTrie, fullTrie);
};

assertEquals(isPossible("a", makeTrie(["a"])), true);
assertEquals(isPossible("a", makeTrie(["b"])), false);
assertEquals(isPossible("ab", makeTrie(["a", "b"])), true);
assertEquals(isPossible("ab", makeTrie(["ab"])), true);
assertEquals(isPossible("ab", makeTrie(["ac"])), false);
assertEquals(isPossible("ab", makeTrie(["a"])), false);
assertEquals(isPossible("ab", makeTrie(["a", "c"])), false);
assertEquals(isPossible("ababab", makeTrie(["ab", "b"])), true);
assertEquals(isPossible("ababbab", makeTrie(["ab", "b"])), true);
assertEquals(isPossible("ababba", makeTrie(["ab", "b"])), false);
assertEquals(
  isPossible(
    "abcdefghijklmnopabcdefghijklmnopabcdefghijklmnop",
    makeTrie(["abcdefghijklmnop"])
  ),
  true
);

const essential = patterns.filter((pattern) =>
  !isPossible(pattern, makeTrie(patterns.filter((p) => p !== pattern)))
);

const possiblePatterns = makeTrie(essential);

assertEquals(
  isPossible(
    "wbgrwubbwguurubrwubuguuwbuubgbugrguwgwrg",
    makeTrie([
      "w",
      "wb",
      "u",
      "wbgrwu",
      "bbw",
      "guur",
      "ubrwubu",
      "guuwb",
      "uub",
      "gbu",
      "grguwg",
      "wrg",
    ])
  ),
  true
);

const printTrie = (trie: PatternTrie, indent = 0) => {
  const entries = Object.entries(trie);
  for (const [key, value] of entries) {
    if (key === ".") continue;
    console.log(" ".repeat(indent) + key);
    if (value !== undefined) printTrie(value, indent + 1);
  }
};

printTrie(possiblePatterns);

const possibleDesigns = designs.filter((design) => {
  console.log(design);
  return isPossible(design, possiblePatterns);
});

console.log(`Part 1: ${possibleDesigns.length}`);
