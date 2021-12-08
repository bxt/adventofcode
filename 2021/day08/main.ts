#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { ensureElementOf, sum } from "../../2020/utils.ts";

const segments = ["a", "b", "c", "d", "e", "f", "g"] as const;

type Segment = typeof segments[number];

type Line = { indicators: Segment[][]; result: Segment[][] };
type Input = Line[];

const parseSegment = (string: string): Segment[] => {
  return string.split("").map((d) => ensureElementOf(d, segments));
};

const numbers = [
  "abcefg",
  "cf",
  "acdeg",
  "acdfg",
  "bcdf",
  "abdfg",
  "abdefg",
  "acf",
  "abcdefg",
  "abcdfg",
].map(parseSegment);

const parseSegmentsList = (string: string): Segment[][] => {
  return string.split(" ").map(parseSegment);
};

const parseInput = (string: string): Input => {
  return string.trim().split("\n").map((line) => {
    const [indicatorsString, resultString] = line.trim().split(" | ");
    return {
      indicators: parseSegmentsList(indicatorsString),
      result: parseSegmentsList(resultString),
    };
  });
};

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

const part1 = (input: Input): number => {
  return sum(
    input.map(({ result }) => {
      const lineCount = result.filter((s) =>
        [2, 4, 3, 7].includes(s.length)
      ).length;
      console.log({ lineCount });
      return lineCount;
    }),
  );
};

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

const getOutputValue = ({ indicators, result }: Line): number => {
  const mapping = Object.fromEntries(
    segments.map((s) => [s, [...segments]]),
  ) as Record<Segment, Segment[]>;
  const numbersMapping: Record<string, number> = {};
  console.log({ mapping });

  const know = (indicator: Segment[], segments: Segment[]) => {
    indicator.forEach((segment) => {
      mapping[segment] = mapping[segment].filter((s) => segments.includes(s));
    });
  };

  const knowNumber = (indicator: Segment[], number: number) => {
    numbersMapping[indicator.sort().join("")] = number;
    know(indicator, numbers[number]);
  };

  const foundSevenOne = (sevenOneIndicator: Segment[]) => {
    indicators.forEach((indicator) => {
      switch (indicator.length) {
        case 2: // has to be a 1
        case 4: // has to be a 4
        case 3: // has to be a 7
        case 7: // has to be a 8
          break;
        case 5: // has to be a 2, 3, 5
          if (sevenOneIndicator.every((s) => indicator.includes(s))) { // has to be a 3
            knowNumber(indicator, 3);
          }
          break;
        case 6: { // has to be a 0, 6, 9
          const notIncluded = sevenOneIndicator.find((s) =>
            !indicator.includes(s)
          );
          if (notIncluded !== undefined) {
            knowNumber(indicator, 6);
            know([notIncluded], ["c"]);
          }
          break;
        }
        default:
          throw new Error(`Huh? ${indicator.length}`);
      }
    });
  };

  indicators.forEach((indicator) => {
    switch (indicator.length) {
      case 2: // has to be a 1
        knowNumber(indicator, 1);
        foundSevenOne(indicator);
        break;
      case 4: // has to be a 4
        knowNumber(indicator, 4);
        break;
      case 3: // has to be a 7
        knowNumber(indicator, 7);
        foundSevenOne(indicator);
        break;
      case 7: // has to be a 8
        knowNumber(indicator, 8);
        break;
      case 5: // has to be a 2, 3, 5
        break;
      case 6: // has to be a 0, 6, 9
        break;
      default:
        throw new Error(`Huh? ${indicator.length}`);
    }
  });

  console.log({ mapping, numbersMapping });

  console.log({ mapping, numbersMapping });

  const decrypt = () => {
    const mapped = result.map((r) =>
      numbersMapping[r.sort().join("")]
    ) as (number | undefined)[];
    console.log({ mapped });
    if (mapped.includes(undefined)) return undefined;
    return parseInt(mapped.join(""), 10);
  };

  const returnValue = decrypt();

  if (returnValue === undefined) throw new Error();

  return returnValue;
};

const part2 = (input: Input): number => {
  return sum(
    input.map(getOutputValue),
  );
};

assertEquals(getOutputValue(exampleLine), 5353);
assertEquals(part2(example), 61229);

console.log("Result part 2: " + part2(input));
