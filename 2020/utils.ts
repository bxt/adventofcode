/**
 * Adds together all numbers in an array.
 * @param {Array<number>} numbers
 * @returns {number} Sum of all numbers
 */
export const sum = (numbers: number[]): number =>
  numbers.reduce((acc, number) => acc + number, 0);

/**
 * Multiplies together all numbers in an array.
 * @param {Array<number>} numbers
 * @returns {number} Product of all numbers
 */
export const product = (numbers: number[]): number =>
  numbers.reduce((acc, number) => acc * number, 1);

/**
 * A basic X/Y coordinate
 */
export type Coord = readonly [number, number];

/**
 * Adds together two coords
 * @param {Coord} a
 * @param {Coord} b
 * @returns {number} Component-wise sum of a and b
 */
export const addCoords = (
  [x1, y1]: Coord,
  [x2, y2]: Coord,
): Coord => ([x1 + x2, y1 + y2]);

/**
 * Scales a coord with a scalar (plain number)
 * @param {Coord} coord
 * @param {number} scalar
 * @returns {Coord} coord with each component scaled by scalar
 */
export const scaleCoord = ([x, y]: Coord, s: number): Coord => ([x * s, y * s]);

/**
 * Calculates the manhatten distance form the coord to the origin (0,0)
 * @param {Coord} coord
 * @returns {number} manhattan distance
 */
export const manhattanNormCoord = ([x, y]: Coord): number =>
  Math.abs(x) + Math.abs(y);

/**
 * Rotates a coord by 90 degrees to the left around the origin (0,0)
 *
 * This assumes that you x axis faces down, otherwise the rotation is right actually...
 *
 * @param {Coord} coord
 * @returns {number} coord rotated
 */
export const rotateLeftNinetyDegreesCoord = (
  [x, y]: Coord,
): Coord => [-y + 0, /* <- convert -0 to 0, lol */ x];

/**
 * Can contain a value or not, usesed to make assertMaybe more semantic.
 */
export type Maybe<T> = T | null;

/**
 * Returns a Maybe with either the value if it is part of the array or null
 * if it is not found in the array. This is used to allow TypeScript to
 * sort array vs. non-array elements on a type level as well.
 */
export const maybeElementOf = <A, B extends A>(
  element: A,
  to: readonly B[],
): Maybe<B> => {
  if (!to.includes(element as B)) {
    return null;
  }

  return element as B;
};

/**
 * Get the maybe value or generates and exception if it's empty.
 */
export const assertMaybe = <T>(
  element: Maybe<T>,
): T => {
  if (element === null) {
    throw new Error(`Maybe: ${element}`);
  }

  return element;
};

/**
 * Returns either the value if it is part of the array or generates an
 * exception if it's not included. Useful for validating inputs.
 */
export const ensureElementOf = <A, B extends A>(
  element: A,
  to: readonly B[],
): B => assertMaybe(maybeElementOf(element, to));

/**
 * Matches a string agains a regex and returns the match groups, or throws
 * and exception if it did not match. Useful for validating inputs.
 */
export const matchGroups = (regexp: RegExp) =>
  (input: string): Record<string, string> => {
    const groups = input.match(regexp)?.groups;

    if (!groups) throw new Error(`Did not match: ${input}`);

    return groups;
  };
