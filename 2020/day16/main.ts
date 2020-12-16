#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.79.0/testing/asserts.ts";
import { matchGroups, sum } from "../utils.ts";

type Range = {
  from: number;
  to: number;
};

type FieldRule = {
  name: string;
  ranges: Range[];
};

type Ticket = number[];

type Input = {
  fieldRules: FieldRule[];
  myTicket: Ticket;
  nearbyTickets: Ticket[];
};

const parseInput = (
  string: string,
): Input => {
  const { fieldRulesString, myTicketString, nearbyTicketsString } = matchGroups(
    /^(?<fieldRulesString>.*)\n\n *your ticket:\n(?<myTicketString>.*)\n\n *nearby tickets:\n(?<nearbyTicketsString>.*)$/s,
  )(
    string.trim(),
  );

  const fieldRules = fieldRulesString.split("\n").map((fieldRuleString) => {
    const {
      name,
      rangeAFromString,
      rangeAToString,
      rangeBFromString,
      rangeBToString,
    } = matchGroups(
      / *(?<name>.*): (?<rangeAFromString>\d*)-(?<rangeAToString>\d*) or (?<rangeBFromString>\d*)-(?<rangeBToString>\d*)/,
    )(
      fieldRuleString,
    );
    return {
      name,
      ranges: [
        { from: Number(rangeAFromString), to: Number(rangeAToString) },
        { from: Number(rangeBFromString), to: Number(rangeBToString) },
      ],
    };
  });

  const myTicket = myTicketString.trim().split(",").map((numberString) =>
    Number(numberString)
  );

  const nearbyTickets = nearbyTicketsString.split("\n").map((ticketString) =>
    ticketString.trim().split(",").map((numberString) => Number(numberString))
  );

  return { fieldRules, myTicket, nearbyTickets };
};

const example = parseInput(`
  class: 1-3 or 5-7
  row: 6-11 or 33-44
  seat: 13-40 or 45-50

  your ticket:
  7,1,14

  nearby tickets:
  7,3,47
  40,4,50
  55,2,20
  38,6,12
`);

assertEquals(example.fieldRules.length, 3);
assertEquals(example.fieldRules[0].name, "class");
assertEquals(
  example.fieldRules[0].ranges,
  [{ from: 1, to: 3 }, { from: 5, to: 7 }],
);
assertEquals(example.myTicket, [7, 1, 14]);
assertEquals(example.nearbyTickets.length, 4);
assertEquals(example.nearbyTickets[0], [7, 3, 47]);
assertEquals(example.nearbyTickets[3], [38, 6, 12]);

const input = await Deno.readTextFile("input.txt");

const inputParsed = parseInput(input);

const isInRange = (number: number, range: Range) =>
  number >= range.from && number <= range.to;

const invalidEntries = (fieldRules: FieldRule[], ticket: number[]): number[] =>
  ticket.filter((number) =>
    !fieldRules.some((rule) =>
      rule.ranges.some((range) => isInRange(number, range))
    )
  );

const part1 = (input: Input): number =>
  sum(
    input.nearbyTickets.flatMap((ticket) =>
      invalidEntries(input.fieldRules, ticket)
    ),
  );

assertEquals(part1(example), 71);

console.log("Result part 1: " + part1(inputParsed));

const example2 = parseInput(`
  class: 0-1 or 4-19
  row: 0-5 or 8-19
  seat: 0-13 or 16-19

  your ticket:
  11,12,13

  nearby tickets:
  3,9,18
  15,1,5
  5,14,9
`);

// Assignment: field index => rule index

const elimintateSingle = (
  possibleAssigments: number[][],
): number[][] | false => {
  const updatedPossibleAssigments = [...possibleAssigments];
  let isAnythingChanged = false;
  for (let i = 0; i < updatedPossibleAssigments.length; i++) {
    if (updatedPossibleAssigments[i].length === 1) {
      for (let k = i + 1; k < updatedPossibleAssigments.length; k++) {
        if (
          updatedPossibleAssigments[k].indexOf(updatedPossibleAssigments[i][0])
        ) {
          isAnythingChanged = true;
          updatedPossibleAssigments[k] = updatedPossibleAssigments[k].filter(
            (ruleIndex) => ruleIndex !== updatedPossibleAssigments[i][0],
          );
        }
      }
    }
  }

  if (!isAnythingChanged) return false;

  return updatedPossibleAssigments;
};

const calculateAssignment = (input: Input): number[] => {
  const filteredNearbyTickets = input.nearbyTickets.filter((ticket) =>
    invalidEntries(input.fieldRules, ticket).length === 0
  );

  console.log({ filteredNearbyTickets });

  const possibleAssigments = input.myTicket.map((_, fieldIndex) =>
    input.fieldRules.flatMap((rule, ruleIndex) => {
      const intialValid = (
        filteredNearbyTickets.every((ticket) => {
            const anyInRange = rule.ranges.some((range) => {
              return isInRange(ticket[fieldIndex], range);
            });
            console.log({ ticket, anyInRange });
            return anyInRange;
          })
          ? [ruleIndex]
          : []
      );
      console.log({ rule, ruleIndex, intialValid });
      return intialValid;
    })
  );

  console.log({ possibleAssigments });

  console.log(elimintateSingle(possibleAssigments));

  return [-1];
};

assertEquals(calculateAssignment(example2), [1, 0, 2]);

const part2 = (input: Input): number => {
  const assignemnt = calculateAssignment(input);

  return -1;
};

console.log("Result part 2: " + part2(inputParsed));
