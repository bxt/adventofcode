#!/usr/bin/env deno run --allow-read
import {
  assert,
  assertEquals,
} from "https://deno.land/std@0.79.0/testing/asserts.ts";
import { getColorEnabled } from "https://deno.land/std@0.79.0/fmt/colors.ts";
import { ensureElementOf, matchGroups } from "../utils.ts";

type NonTerminal = number;
const terminals = ["a", "b"] as const;
type Terminal = typeof terminals[number];
type Rule =
  & { nonTerminal: NonTerminal }
  & ({ terminal: Terminal } | { nonTerminals: [NonTerminal, NonTerminal] });
type Grammar = Rule[];
type Word = Terminal[];

const range = (length: number): number[] =>
  Array(length).fill(null).map((_, n) => n);

const parseInput = (string: string): { grammar: Grammar; words: Word[] } => {
  const [grammarString, wordsString] = string.trim().split("\n\n");

  const nonTerminalList = grammarString.split("\n").map((ruleString) =>
    Number(ruleString.split(": ")[0])
  ).sort((a, b) => a - b);
  assertEquals(
    range(nonTerminalList.length),
    nonTerminalList,
  );
  let nextUnusedNonTerminal = nonTerminalList.length;
  const aliases: [NonTerminal, NonTerminal][] = [];

  const grammar: Grammar = grammarString.split("\n").flatMap((ruleString) => {
    const [nonTerminalString, productions] = ruleString.split(": ");
    const nonTerminal = Number(nonTerminalString);

    const groups = matchGroups(
      /("(?<terminal>[ab])")|(?<productions>[\d |]*)/,
    )(productions);
    if (groups.terminal) {
      return [
        { nonTerminal, terminal: ensureElementOf(groups.terminal, terminals) },
      ];
    } else {
      const productionsStrings = groups.productions.split(" | ");
      return productionsStrings.flatMap((productionsString) => {
        const nonTerminals = productionsString.split(" ").map(Number);
        if (nonTerminals.length === 2) {
          return [{
            nonTerminal,
            nonTerminals: [nonTerminals[0], nonTerminals[1]],
          }];
        } else if (nonTerminals.length === 1) {
          aliases.push([nonTerminal, nonTerminals[0]]);
          return []; // deal with later
        } else { // > 2
          const rules: Rule[] = [];
          rules.push(
            {
              nonTerminal,
              nonTerminals: [nonTerminals[0], nextUnusedNonTerminal],
            },
          );
          nextUnusedNonTerminal++;
          for (let i = 0; i < nonTerminals.length - 3; i++) {
            rules.push(
              {
                nonTerminal: nextUnusedNonTerminal - 1,
                nonTerminals: [nonTerminals[i + 1], nextUnusedNonTerminal],
              },
            );
            nextUnusedNonTerminal++;
          }
          rules.push(
            {
              nonTerminal: nextUnusedNonTerminal - 1,
              nonTerminals: [
                nonTerminals[nonTerminals.length - 2],
                nonTerminals[nonTerminals.length - 1],
              ],
            },
          );
          return rules;
        }
      });
    }
  });

  aliases.forEach(([obsoleteNonTerminal, insteadNonTerminal]) => {
    grammar.forEach((rule) => {
      if ("nonTerminals" in rule) {
        rule.nonTerminals = rule.nonTerminals.map((nonTerminal) => {
          return nonTerminal === obsoleteNonTerminal
            ? insteadNonTerminal
            : nonTerminal;
        }) as [NonTerminal, NonTerminal];
      }
    });
  });

  const words = wordsString.split(/\n/).map((wordString) =>
    wordString.trim().split("").map((t) => ensureElementOf(t, terminals))
  );

  return { grammar, words };
};

const example = parseInput(`
  0: 4 1 5
  1: 2 3 | 3 2
  2: 4 4 | 5 5
  3: 4 5 | 5 4
  4: "a"
  5: "b"

  ababbb
  bababa
  abbbab
  aaabbb
  aaaabbb
`);

assertEquals(example.words.length, 5);
assertEquals(example.words[0], ["a", "b", "a", "b", "b", "b"]);
const rule4 = example.grammar.filter((rule) => rule.nonTerminal === 4)[0];
assert("terminal" in rule4);
assertEquals(rule4.terminal, "a");
const rule0 = example.grammar.filter((rule) => rule.nonTerminal === 0)[0];
assert("nonTerminals" in rule0);
assertEquals(rule0.nonTerminals, [4, 6]);
const rule6 = example.grammar.filter((rule) => rule.nonTerminal === 6)[0];
assert("nonTerminals" in rule6);
assertEquals(rule6.nonTerminals, [1, 5]);

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

// Cocke–Younger–Kasami algorithm
const isWordInGrammar = (word: Word, grammar: Grammar) => {
  // matches indexed by [spanLength - 1][spanStart][nonTerminal]
  const partialMatches: Record<NonTerminal, boolean>[][] = range(word.length)
    .map((spanLengthMinusOne) =>
      range(word.length - spanLengthMinusOne).map((_) => ({}))
    );

  for (let i = 0; i < word.length; i++) {
    const spanLength = 1;
    const rule = grammar.find((rule) =>
      "terminal" in rule && rule.terminal === word[i]
    );
    if (!rule) throw new Error(`No rule for terminal ${word[i]}`);
    partialMatches[spanLength - 1][i][rule.nonTerminal] = true;
  }

  for (let spanLength = 2; spanLength <= word.length; spanLength++) {
    for (
      let spanStart = 0;
      spanStart <= word.length - spanLength;
      spanStart++
    ) {
      for (
        let spanPartition = 1;
        spanPartition < spanLength;
        spanPartition++
      ) {
        grammar.forEach((rule) => {
          if ("nonTerminals" in rule) {
            const { nonTerminal, nonTerminals: [nonTerminalA, nonTerminalB] } =
              rule;
            const matchesA =
              partialMatches[spanPartition - 1][spanStart][nonTerminalA];
            const matchesB = partialMatches[spanLength - spanPartition - 1][
              spanStart + spanPartition
            ][nonTerminalB];
            if (matchesA && matchesB) {
              partialMatches[spanLength - 1][spanStart][nonTerminal] = true;
            }
          }
        });
      }
    }
  }

  return partialMatches[word.length - 1][0][0];
};

assertEquals(
  isWordInGrammar(["a", "b", "a", "b", "b", "b"], example.grammar),
  true,
);
assertEquals(
  isWordInGrammar(["a", "b", "b", "b", "a", "b"], example.grammar),
  true,
);

const part1 = (
  { grammar, words }: { grammar: Grammar; words: Word[] },
): number => words.filter((word) => isWordInGrammar(word, grammar)).length;

assertEquals(part1(example), 2);

const exampleWithAliases = parseInput(`
  0: 5 2 6
  1: 6
  2: 3 4 | 4 3
  3: 7 5 | 1 6
  4: 5 1 | 6 5
  5: "a"
  6: "b"
  7: 5

  ababbb
  bababa
  abbbab
  aaabbb
  aaaabbb
`);

assertEquals(
  isWordInGrammar(["a", "b", "a", "b", "b", "b"], exampleWithAliases.grammar),
  true,
);
assertEquals(
  isWordInGrammar(["a", "b", "b", "b", "a", "b"], exampleWithAliases.grammar),
  true,
);
assertEquals(part1(exampleWithAliases), 2);

inputParsed.grammar.forEach((rule) => {
  console.log(
    `${rule.nonTerminal}: ${
      ("terminal" in rule)
        ? `"${rule.terminal}"`
        : rule.nonTerminals.map(String).join(" ")
    }`,
  );
});

console.log("Result part 1: " + part1(inputParsed));
// 105 too low
