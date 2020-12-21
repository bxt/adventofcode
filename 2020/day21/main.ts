#!/usr/bin/env deno run --allow-read
import {
  assert,
  assertEquals,
} from "https://deno.land/std@0.79.0/testing/asserts.ts";
import {
  addCoords,
  boundsOfCoords,
  Coord,
  CoordArray,
  ensureElementOf,
  indexWithCoord,
  matchGroups,
  product,
  range,
  rangeCoords,
  scaleCoord,
  SparseCoordArray,
  sum,
} from "../utils.ts";

type Ingredient = string;
type Allergen = string;

const parseInput = (string: string): [Ingredient[], Allergen[]][] =>
  string.trim().split("\n").map((line) => {
    const { ingredientsString, allergensString } = matchGroups(
      /(?<ingredientsString>.*) \(contains (?<allergensString>.*)\)/,
    )(line.trim());

    const ingredients = ingredientsString.split(" ").map((i) => i.trim());
    const allergens = allergensString.split(", ").map((a) => a.trim());

    return [ingredients, allergens] as [Ingredient[], Allergen[]];
  });

const input = await Deno.readTextFile("input.txt");

const parsedInput = parseInput(input);

const intersectSets = <T>(as: Set<T>, bs: Set<T>): Set<T> =>
  new Set([...as].filter((a) => bs.has(a)));

const minusSets = <T>(as: Set<T>, bs: Set<T>): Set<T> =>
  new Set([...as].filter((a) => !bs.has(a)));

const calculateAllergenPossibilities = (
  ingredientsAndAllergens: [Ingredient[], Allergen[]][],
): Record<Allergen, Set<Ingredient>> => {
  const allergens = new Set(ingredientsAndAllergens.flatMap(([_, as]) => as));

  const allergenSets: Record<Allergen, Set<Ingredient>[]> = Object
    .fromEntries([...allergens].map((a) => [a, []]));

  ingredientsAndAllergens.forEach(([ingredients, allergens]) => {
    const ingredientSet = new Set([...ingredients]);
    allergens.forEach((allergen) => {
      allergenSets[allergen].push(ingredientSet);
    });
  });

  const allergenPossibilities: Record<Allergen, Set<Ingredient>> = Object
    .fromEntries(
      Object.entries(allergenSets).map((
        [a, sets],
      ) => [a, sets.reduce(intersectSets)]),
    );

  return allergenPossibilities;
};

const part1 = (
  ingredientsAndAllergens: [Ingredient[], Allergen[]][],
): number => {
  const ingredients = new Set(ingredientsAndAllergens.flatMap(([is, _]) => is));

  const allergenPossibilities: Record<Allergen, Set<Ingredient>> =
    calculateAllergenPossibilities(ingredientsAndAllergens);

  const ingredientsWithoutOneAllergen = Object.values(allergenPossibilities)
    .reduce(minusSets, ingredients);

  const ingredientsWithOneAllergenAppearances = sum(
    ingredientsAndAllergens.map(([is, _]) =>
      is.filter((i) => ingredientsWithoutOneAllergen.has(i)).length
    ),
  );

  return ingredientsWithOneAllergenAppearances;
};

const example = parseInput(`
  mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
  trh fvjkl sbzzf mxmxvkd (contains dairy)
  sqjhc fvjkl (contains soy)
  sqjhc mxmxvkd sbzzf (contains fish)
`);

assertEquals(part1(example), 5);

console.log("Result part 1: " + part1(parsedInput));
