#!/usr/bin/env deno run --allow-read
import {
  assert,
  assertEquals,
} from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { ensureElementOf, matchGroups, range } from "../utils.ts";

type NonTerminal = number;

const terminals = ["a", "b"] as const;
type Terminal = typeof terminals[number];

type RuleBase = { nonTerminal: NonTerminal };
type TerminalRule = RuleBase & { terminal: Terminal };
type NonTerminalsRule = RuleBase & { nonTerminals: [NonTerminal, NonTerminal] };
type AliasRule = RuleBase & { alias: NonTerminal };
type Rule = TerminalRule | NonTerminalsRule | AliasRule;

type Grammar = Rule[];
type Word = Terminal[];

type ParsedInput = { grammar: Grammar; words: Word[] };

const parseWord = (wordString: string): Word =>
  wordString.trim().split("").map((t) => ensureElementOf(t, terminals));

const parseInput = (string: string): ParsedInput => {
  const [grammarString, wordsString] = string.trim().split("\n\n");

  let nextUnusedNonTerminal = Math.max(
    ...grammarString.split("\n").map((ruleString) =>
      Number(ruleString.split(": ")[0])
    ),
  ) + 1;

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
          return [{
            nonTerminal,
            alias: nonTerminals[0],
          }];
        } else { // > 2
          const rules: Rule[] = [];
          rules.push({
            nonTerminal,
            nonTerminals: [nonTerminals[0], nextUnusedNonTerminal],
          });
          nextUnusedNonTerminal++;
          for (let i = 0; i < nonTerminals.length - 3; i++) {
            rules.push({
              nonTerminal: nextUnusedNonTerminal - 1,
              nonTerminals: [nonTerminals[i + 1], nextUnusedNonTerminal],
            });
            nextUnusedNonTerminal++;
          }
          rules.push({
            nonTerminal: nextUnusedNonTerminal - 1,
            nonTerminals: [
              nonTerminals[nonTerminals.length - 2],
              nonTerminals[nonTerminals.length - 1],
            ],
          });
          return rules;
        }
      });
    }
  });

  const words = wordsString.split(/\n/).map(parseWord);

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
assertEquals(example.words[0], parseWord("ababbb"));

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

// Cocke–Younger–Kasami algorithm (CYK), but can also handle A -> B
const isWordInGrammar = (word: Word, grammar: Grammar) => {
  const terminalRules: TerminalRule[] = [];
  const aliasRules: AliasRule[] = [];
  const nonTerminalRules: NonTerminalsRule[] = [];
  grammar.forEach((rule) => {
    if ("terminal" in rule) {
      terminalRules.push(rule);
    } else if ("alias" in rule) {
      aliasRules.push(rule);
    } else {
      nonTerminalRules.push(rule);
    }
  });

  // matches indexed by [spanLength - 1][spanStart][nonTerminal]
  const partialMatches: Record<NonTerminal, boolean>[][] = range(word.length)
    .map((spanLengthMinusOne) =>
      range(word.length - spanLengthMinusOne).map((_) => ({}))
    );

  const handleAliases = (
    { spanLength, spanStart }: { spanLength: number; spanStart: number },
  ): void => {
    aliasRules.forEach((rule) => {
      const { nonTerminal, alias } = rule;
      const matchesAlias = partialMatches[spanLength - 1][spanStart][alias];
      if (matchesAlias) {
        partialMatches[spanLength - 1][spanStart][nonTerminal] = true;
      }
    });
  };

  for (let spanStart = 0; spanStart < word.length; spanStart++) {
    const spanLength = 1;
    const rule = terminalRules.find((rule) =>
      rule.terminal === word[spanStart]
    );
    if (!rule) throw new Error(`No rule for terminal ${word[spanStart]}`);
    partialMatches[spanLength - 1][spanStart][rule.nonTerminal] = true;

    handleAliases({ spanLength, spanStart });
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
        nonTerminalRules.forEach((rule) => {
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
        });
      }

      handleAliases({ spanLength, spanStart });
    }
  }

  return partialMatches[word.length - 1][0][0];
};

assertEquals(isWordInGrammar(parseWord("ababbb"), example.grammar), true);
assertEquals(isWordInGrammar(parseWord("abbbab"), example.grammar), true);

const countWordsInGrammar = ({ grammar, words }: ParsedInput): number =>
  words.filter((word) => isWordInGrammar(word, grammar)).length;

assertEquals(countWordsInGrammar(example), 2);

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
  isWordInGrammar(parseWord("ababbb"), exampleWithAliases.grammar),
  true,
);
assertEquals(
  isWordInGrammar(parseWord("abbbab"), exampleWithAliases.grammar),
  true,
);
assertEquals(countWordsInGrammar(exampleWithAliases), 2);

const allPossibilities = (
  grammar: Grammar,
  nonTerminal: NonTerminal,
): string[] => {
  const cache: Record<NonTerminal, string[]> = {};
  const recursion = (nonTerminal: NonTerminal): string[] => {
    if (cache[nonTerminal]) return cache[nonTerminal];

    const possibilities = grammar.filter((rule) =>
      rule.nonTerminal === nonTerminal
    ).flatMap((rule) => {
      if ("terminal" in rule) {
        return [rule.terminal];
      } else if ("alias" in rule) {
        return recursion(rule.alias);
      } else {
        const [a, b] = rule.nonTerminals;
        const possibilitiesForA = recursion(a);
        const possibilitiesForB = recursion(b);
        return possibilitiesForA.flatMap((pa) =>
          possibilitiesForB.map((pb) => pa + pb)
        );
      }
    });
    cache[nonTerminal] = possibilities;
    return possibilities;
  };
  return recursion(nonTerminal);
};

const checkPossibilities = (grammar: Grammar) => {
  const possibilities = allPossibilities(grammar, 0);
  possibilities.forEach((possibility) => {
    assert(isWordInGrammar(parseWord(possibility), grammar));
  });
};

checkPossibilities(exampleWithAliases.grammar);

assertEquals(inputParsed.words.length, 468);

const part1Result = countWordsInGrammar(inputParsed);

assertEquals(
  (() => {
    const possibilities = allPossibilities(inputParsed.grammar, 0);
    return inputParsed.words.map((w) => w.join("")).filter((w) =>
      possibilities.includes(w)
    ).length;
  })(),
  part1Result,
);

console.log("Result part 1: " + part1Result);

const prepareForPart2 = (parsedInput: ParsedInput) => {
  const nextUnusedNonTerminal = Math.max(
    ...parsedInput.grammar.map((rule) => rule.nonTerminal),
  ) + 1;

  let rule8found = false;
  let rule11found = false;

  const grammar = parsedInput.grammar.flatMap((rule) => {
    if (rule.nonTerminal === 8) {
      assert("alias" in rule);
      assertEquals(rule.alias, 42);
      rule8found = true;
      return [
        { nonTerminal: 8, alias: 42 },
        { nonTerminal: 8, nonTerminals: [42, 8] },
      ] as Rule[];
    } else if (rule.nonTerminal === 11) {
      assert("nonTerminals" in rule);
      assertEquals(rule.nonTerminals, [42, 31]);
      rule11found = true;
      return [
        { nonTerminal: 11, nonTerminals: [42, 31] },
        { nonTerminal: 11, nonTerminals: [42, nextUnusedNonTerminal] },
        { nonTerminal: nextUnusedNonTerminal, nonTerminals: [11, 31] },
      ] as Rule[];
    } else {
      return [rule];
    }
  });
  assert(rule8found);
  assert(rule11found);

  return { ...parsedInput, grammar };
};

const example2 = parseInput(`
  42: 9 14 | 10 1
  9: 14 27 | 1 26
  10: 23 14 | 28 1
  1: "a"
  11: 42 31
  5: 1 14 | 15 1
  19: 14 1 | 14 14
  12: 24 14 | 19 1
  16: 15 1 | 14 14
  31: 14 17 | 1 13
  6: 14 14 | 1 14
  2: 1 24 | 14 4
  0: 8 11
  13: 14 3 | 1 12
  15: 1 | 14
  17: 14 2 | 1 7
  23: 25 1 | 22 14
  28: 16 1
  4: 1 1
  20: 14 14 | 1 15
  3: 5 14 | 16 1
  27: 1 6 | 14 18
  14: "b"
  21: 14 1 | 1 14
  25: 1 1 | 1 14
  22: 14 14
  8: 42
  26: 14 22 | 1 20
  18: 15 15
  7: 14 5 | 1 21
  24: 14 1

  abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
  bbabbbbaabaabba
  babbbbaabbbbbabbbbbbaabaaabaaa
  aaabbbbbbaaaabaababaabababbabaaabbababababaaa
  bbbbbbbaaaabbbbaaabbabaaa
  bbbababbbbaaaaaaaabbababaaababaabab
  ababaaaaaabaaab
  ababaaaaabbbaba
  baabbaaaabbaaaababbaababb
  abbbbabbbbaaaababbbbbbaaaababb
  aaaaabbaabaaaaababaa
  aaaabbaaaabbaaa
  aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
  babaaabbbaaabaababbaabababaaab
  aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba
`);

assertEquals(countWordsInGrammar(example2), 3);

const example2prepared = prepareForPart2(example2);

const rule8s = example2prepared.grammar.filter((rule) =>
  rule.nonTerminal === 8
);
assertEquals(rule8s.length, 2);
rule8s.forEach((rule8) => {
  if ("alias" in rule8) {
    assertEquals(rule8.alias, 42);
  }
  if ("nonTerminals" in rule8) {
    assertEquals(rule8.nonTerminals, [42, 8]);
  }
});

assertEquals(countWordsInGrammar(example2prepared), 12);

console.log(
  "Result part 2: " + countWordsInGrammar(prepareForPart2(parseInput(input))),
);
