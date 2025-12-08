#!/usr/bin/env deno run --allow-read
import { assertEquals } from "jsr:@std/assert@1.0.16";
import { ensureElementOf, sum } from "../../2020/utils.ts";

const segments = ["a", "b", "c", "d", "e", "f", "g"] as const;

type Segment = typeof segments[number];

type Indicator = Segment[];
type Line = { indicators: Indicator[]; result: Indicator[] };
type Input = Line[];

const parseIndicator = (string: string): Indicator =>
  string.split("").map((d) => ensureElementOf(d, segments));

const parseIndicatorList = (string: string): Indicator[] =>
  string.split(" ").map(parseIndicator);

const parseInput = (string: string): Input =>
  string.trim().split("\n").map((line) => {
    const [indicatorsString, resultString] = line.trim().split(" | ");
    return {
      indicators: parseIndicatorList(indicatorsString),
      result: parseIndicatorList(resultString),
    };
  });

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

const part1 = (input: Input): number =>
  sum(
    input.map(({ result }) =>
      result.filter((s) => [2, 4, 3, 7].includes(s.length)).length
    ),
  );

const example = parseInput(`
  be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
  edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
  fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
  fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
  aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
  fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
  dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
  bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
  egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
  gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
`);

const exampleLine = parseInput(`
  acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
`)[0];

assertEquals(part1(example), 26);

console.log("Result part 1: " + part1(input));

const normalizeIndicator = (indicator: Indicator): string =>
  indicator.sort().join("");

const findAndRemove = <T>(
  array: T[],
  predicate: (t: T) => boolean,
): T | undefined => {
  const element = array.find(predicate);
  if (element === undefined) return undefined;
  array.splice(array.indexOf(element), 1);
  return element;
};

const findMapping = (indicators: Indicator[]): Record<string, number> => {
  const numbersMapping: Record<string, number> = {};
  const indicatorsMapping: Record<number, Indicator> = {};

  const knowNumber = (number: number, indicator: Indicator | undefined) => {
    if (indicator === undefined) {
      throw new Error(`Indicator for ${number} not found!`);
    }
    numbersMapping[normalizeIndicator(indicator)] = number;
    indicatorsMapping[number] = indicator;
  };

  if (indicators.length !== 10) {
    throw new Error("Missing indicator for a digit?");
  }

  const indicatorsLength5: Indicator[] = [];
  const indicatorsLength6: Indicator[] = [];

  indicators.forEach((indicator) => {
    switch (indicator.length) {
      case 2:
        knowNumber(1, indicator);
        break;
      case 4:
        knowNumber(4, indicator);
        break;
      case 3:
        knowNumber(7, indicator);
        break;
      case 7:
        knowNumber(8, indicator);
        break;
      case 5: // can be a 2, 3, 5
        indicatorsLength5.push(indicator);
        break;
      case 6: // can be a 0, 6, 9
        indicatorsLength6.push(indicator);
        break;
      default:
        throw new Error(`Unexpected indicator length: ${indicator.length}`);
    }
  });

  [1, 4, 7, 8].forEach((number) => {
    if (indicatorsMapping[number] === undefined) {
      throw new Error(`Indicator for ${number} not found?`);
    }
  });
  if (indicatorsLength5.length !== 3) throw new Error("Wrong digits? (5)");
  if (indicatorsLength6.length !== 3) throw new Error("Wrong digits? (6)");

  const hasAllFrom = (number: number) =>
    (indicator: Indicator) =>
      indicatorsMapping[number].every((s) => indicator.includes(s));

  // 3 is the only one with 6 segments and all segments of 1:
  knowNumber(3, findAndRemove(indicatorsLength5, hasAllFrom(1)));

  let mapsToSegmentC: Segment | undefined = undefined;
  knowNumber(
    6, // is the only one with 6 segments and not all segments of 1:
    findAndRemove(indicatorsLength6, (indicator) => {
      const notIncluded = indicatorsMapping[1].find((s) =>
        !indicator.includes(s)
      );
      if (notIncluded === undefined) return false;
      mapsToSegmentC = notIncluded;
      return true;
    }),
  );

  knowNumber(
    2, // has the segment "c" (other than 5)
    findAndRemove(indicatorsLength5, (indicator) => {
      if (mapsToSegmentC === undefined) throw new Error("Unreachable");
      return indicator.includes(mapsToSegmentC);
    }),
  );

  knowNumber(5, indicatorsLength5.pop());

  // 9 has all segments of 4 (other than 0)
  knowNumber(9, findAndRemove(indicatorsLength6, hasAllFrom(4)));

  knowNumber(0, indicatorsLength6.pop());

  return numbersMapping;
};

const getLineValue = ({ indicators, result }: Line): number => {
  const numbersMapping = findMapping(indicators);

  const mapped = result.map((r) => numbersMapping[normalizeIndicator(r)]);

  return parseInt(mapped.join(""), 10);
};

const part2 = (input: Input): number => sum(input.map(getLineValue));

assertEquals(getLineValue(exampleLine), 5353);
assertEquals(part2(example), 61229);

console.log("Result part 2: " + part2(input));
