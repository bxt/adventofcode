#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { intersectSets, matchGroups, minusSets, sum } from "../utils.ts";

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
      ) => [a, intersectSets(...sets)]),
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

const elimintateSingle = (
  allergenPossibilities: Record<Allergen, Set<Ingredient>>,
): boolean => {
  let isAnythingChanged = false;

  Object.entries(allergenPossibilities).forEach(([a1, is1]) => {
    if (is1.size === 1) {
      const [i] = is1.values();
      Object.entries(allergenPossibilities).forEach(([a2, is2]) => {
        if (a1 !== a2 && is2.has(i)) {
          allergenPossibilities[a2] = minusSets(is2, is1);
          isAnythingChanged = true;
        }
      });
    }
  });

  return isAnythingChanged;
};

const part2 = (
  ingredientsAndAllergens: [Ingredient[], Allergen[]][],
): string => {
  const allergenPossibilities: Record<Allergen, Set<Ingredient>> =
    calculateAllergenPossibilities(ingredientsAndAllergens);

  // deno-lint-ignore no-empty
  while (elimintateSingle(allergenPossibilities)) {
  }

  const isAllSolved = Object.values(allergenPossibilities).every((is) =>
    is.size === 1
  );
  if (!isAllSolved) throw new Error("Not implmented");

  const sortedAllergenPossibilities = Object.entries(allergenPossibilities)
    .sort(([a1], [a2]) => a1[0].localeCompare(a2[0]));

  const canonicalDangerousIngredientList = sortedAllergenPossibilities.map((
    [_, [i]],
  ) => i).join(",");

  return canonicalDangerousIngredientList;
};

assertEquals(part2(example), "mxmxvkd,sqjhc,fvjkl");

console.log("Result part 2: " + part2(parsedInput));
