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

const findAndRemove = <T>(array: T[], predicate: (t: T) => boolean): T => {
  const element = array.find(predicate);
  if (element === undefined) throw new Error("Not found");
  array.splice(array.indexOf(element), 1);
  return element;
};

const getLineValue = ({ indicators, result }: Line): number => {
  const numbersMapping: Record<string, number> = {};
  const indicatorsMapping: Record<number, Segment[]> = {};

  const knowNumber = (indicator: Segment[], number: number) => {
    numbersMapping[indicator.sort().join("")] = number;
    indicatorsMapping[number] = indicator;
  };

  if (indicators.length !== 10) {
    throw new Error("Missing indicator for a digit?");
  }

  const indicatorsLength5: Segment[][] = [];
  const indicatorsLength6: Segment[][] = [];

  indicators.forEach((indicator) => {
    switch (indicator.length) {
      case 2: // has to be a 1
        knowNumber(indicator, 1);
        break;
      case 4: // has to be a 4
        knowNumber(indicator, 4);
        break;
      case 3: // has to be a 7
        knowNumber(indicator, 7);
        break;
      case 7: // has to be a 8
        knowNumber(indicator, 8);
        break;
      case 5: // has to be a 2, 3, 5
        indicatorsLength5.push(indicator);
        break;
      case 6: // has to be a 0, 6, 9
        indicatorsLength6.push(indicator);
        break;
      default:
        throw new Error(`Unexpected indicator length: ${indicator.length}`);
    }
  });

  if (indicatorsMapping[1] === undefined) throw new Error();
  if (indicatorsLength5.length !== 3) throw new Error();
  if (indicatorsLength6.length !== 3) throw new Error();

  const threeIndicator = findAndRemove(indicatorsLength5, (indicator) => {
    return indicatorsMapping[1].every((s) => indicator.includes(s));
  });
  knowNumber(threeIndicator, 3);

  let mapsToSegmentC: Segment | undefined = undefined;
  const sixIndicator = findAndRemove(indicatorsLength6, (indicator) => {
    const notIncluded = indicatorsMapping[1].find((s) =>
      !indicator.includes(s)
    );
    if (notIncluded === undefined) return false;
    mapsToSegmentC = notIncluded;
    return true;
  });
  knowNumber(sixIndicator, 6);

  if (mapsToSegmentC === undefined) throw new Error();

  const twoIndicator = findAndRemove(indicatorsLength5, (indicator) => {
    if (mapsToSegmentC === undefined) throw new Error();
    return indicator.includes(mapsToSegmentC);
  });
  knowNumber(twoIndicator, 2);

  const [fiveIndicator] = indicatorsLength5;
  knowNumber(fiveIndicator, 5);

  const nineIndicator = findAndRemove(indicatorsLength6, (indicator) => {
    return indicatorsMapping[4].every((s) => indicator.includes(s));
  });
  knowNumber(nineIndicator, 9);

  const [zeroIndicator] = indicatorsLength6;
  knowNumber(zeroIndicator, 0);

  console.log({ numbersMapping });

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
    input.map(getLineValue),
  );
};

assertEquals(getLineValue(exampleLine), 5353);
assertEquals(part2(example), 61229);

console.log("Result part 2: " + part2(input));
