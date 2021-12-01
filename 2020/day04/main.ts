#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";

type Passport = Record<string, string>;

const parseInput = (string: string): Passport[] =>
  string.trim().split("\n\n").map((block) => (
    block.trim().split(/[ \n]+/).reduce((acc, field) => {
      const [key, value] = field.split(":");
      return { ...acc, [key]: value };
    }, {})
  ));

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

const example = parseInput(`
  ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
  byr:1937 iyr:2017 cid:147 hgt:183cm

  iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
  hcl:#cfa07d byr:1929

  hcl:#ae17e1 iyr:2013
  eyr:2024
  ecl:brn pid:760753108 byr:1931
  hgt:179cm

  hcl:#cfa07d eyr:2025 pid:166559648
  iyr:2011 ecl:brn hgt:59in
`);

const isValid = (passport: Passport) =>
  Object.keys(passport).filter((k) => k !== "cid").length === 7;

const part1 = (inputs: Passport[]) => inputs.filter(isValid).length;

assertEquals(part1(example), 2, "Example is wrong!");

console.log("Result part 1: " + part1(inputParsed));

const between = (number: number, min: number, max: number): boolean =>
  number >= min && number <= max;

const isHeightValid = (height: string): boolean =>
  Boolean(height) &&
  (height.endsWith("in")
    ? between(Number(height.slice(0, -2)), 59, 76)
    : height.endsWith("cm")
    ? between(Number(height.slice(0, -2)), 150, 193)
    : false);

const isValuesValid = (passport: Passport) => {
  const { byr, iyr, eyr, hgt, hcl, ecl, pid } = passport;

  if (byr?.length !== 4) return false;
  if (!between(Number(byr), 1920, 2002)) return false;

  if (iyr?.length !== 4) return false;
  if (!between(Number(iyr), 2010, 2020)) return false;

  if (eyr?.length !== 4) return false;
  if (!between(Number(eyr), 2020, 2030)) return false;

  if (!isHeightValid(hgt)) return false;

  if (!hcl?.match(/^#[0-9a-f]{6}$/)) return false;

  if (
    !["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].includes(ecl)
  ) {
    return false;
  }

  if (!pid?.match(/^[0-9]{9}$/)) return false;

  return true;
};

const example2 = parseInput(`
  eyr:1972 cid:100
  hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

  iyr:2019
  hcl:#602927 eyr:1967 hgt:170cm
  ecl:grn pid:012533040 byr:1946

  hcl:dab227 iyr:2012
  ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

  hgt:59cm ecl:zzz
  eyr:2038 hcl:74454a iyr:2023
  pid:3556412378 byr:2007
`);

const example3 = parseInput(`
  pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
  hcl:#623a2f

  eyr:2029 ecl:blu cid:129 byr:1989
  iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

  hcl:#888785
  hgt:164cm byr:2001 iyr:2015 cid:88
  pid:545766238 ecl:hzl
  eyr:2022

  iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
`);

const part2 = (inputs: Passport[]) =>
  inputs.filter(isValid).filter(isValuesValid).length;

assertEquals(part2(example2), 0, "Example is wrong!");
assertEquals(part2(example3), 4, "Example is wrong!");

console.log("Result part 2: " + part2(inputParsed));
