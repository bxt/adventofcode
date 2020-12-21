/**
 * 2020 TypeScript Utilities
 *
 * This contains a bunch of utilities I used for solivng the 2020 riddles.
 *
 */

/**
 * Adds together all numbers in an array and returns their sum.
 */
export function sum(numbers: number[]): number {
  return numbers.reduce((acc, number) => acc + number, 0);
}

/**
 * Multiplies together all numbers in an array and returns their product.
 */
export function product(numbers: number[]): number {
  return numbers.reduce((acc, number) => acc * number, 1);
}

/**
 * Finds the minimum and maximum numbers in an array or any iterable.
 */
export function minMax(numbers: Iterable<number>): [number, number] {
  return [Math.min(...numbers), Math.max(...numbers)];
}

/**
 * Generates a list of `length` numbers from 0 to `length - 1`.
 */
export function range(length: number): number[] {
  return Array(length).fill(null).map((_, n) => n);
}

/**
 * A basic X/Y coordinate
 */
export type Coord = readonly [number, number];

/**
 * An array that can be index by a coord, with y in the outer index and
 * x in the inner index, like `array[y][x]`.
 */
export type CoordArray<T> = T[][];

/**
 * A dictionary that can be index by a coord, with y in the outer index
 * and x in the inner index, like `object[y][x]`.
 */
export type SparseCoordArray<T> = Record<number, Record<number, T>>;

/**
 * Adds together two coords and returns their component-wise sum.
 */
export function addCoords([x1, y1]: Coord, [x2, y2]: Coord): Coord {
  return ([x1 + x2, y1 + y2]);
}

/**
 * Scales a coord with a scalar (plain number), multiplies each component.
 */
export function scaleCoord([x, y]: Coord, s: number): Coord {
  return ([x * s, y * s]);
}

/**
 * Calculates the manhatten distance form the coord to the origin `(0,0)`.
 */
export function manhattanNormCoord([x, y]: Coord): number {
  return Math.abs(x) + Math.abs(y);
}

/**
 * Rotates a coord by 90 degrees to the left around the origin `(0,0)`.
 *
 * This assumes that you x axis faces down, otherwise the rotation is right actually...
 */
export function rotateLeftNinetyDegreesCoord([x, y]: Coord): Coord {
  return [-y + 0, /* <- convert -0 to 0, lol */ x];
}

/**
 * Finds the element at the specfied coords in a
 * `CoordArray` or `SparseCoordArray`.
 *
 * This again assumes that the indexing happens by y first, then x.
 */
export function indexWithCoord<T>(
  array: CoordArray<T> | SparseCoordArray<T>,
  coord: Coord,
): T {
  const [x, y] = coord;
  return array[y][x];
}

/**
 * Finds the minium and maximum values for each coord component in
 * a list of coords.
 */
export function boundsOfCoords(coords: Coord[]): [Coord, Coord] {
  const xs = coords.map((coord) => coord[0]);
  const ys = coords.map((coord) => coord[1]);
  const [minX, maxX] = minMax(xs);
  const [minY, maxY] = minMax(ys);
  return [[minX, minY], [maxX, maxY]];
}

/**
 * Generate a list of coords form `(0, 0)` up to but not including `extend`.
 *
 * The coords will start with `(0, 0)`, `(1, 0)`, `(2, 0)`...
 */
export function rangeCoords(extend: Coord): Coord[] {
  return range(extend[1]).flatMap((y) => range(extend[0]).map((x) => [x, y]));
}

/**
 * Can contain a value or not, usesed to make assertMaybe more semantic.
 */
export type Maybe<T> = T | null;

/**
 * Returns a Maybe with either the value if it is part of the array or null
 * if it is not found in the array. This is used to allow TypeScript to
 * sort array vs. non-array elements on a type level as well.
 */
export function maybeElementOf<A, B extends A>(
  element: A,
  to: readonly B[],
): Maybe<B> {
  if (!to.includes(element as B)) {
    return null;
  }

  return element as B;
}

/**
 * Get the maybe value or generates and exception if it's empty.
 */
export function assertMaybe<T>(element: Maybe<T>): T {
  if (element === null) {
    throw new Error(`Maybe: ${element}`);
  }

  return element;
}

/**
 * Returns either the value if it is part of the array or generates an
 * exception if it's not included. Useful for validating inputs.
 */
export function ensureElementOf<A, B extends A>(
  element: A,
  to: readonly B[],
): B {
  return assertMaybe(maybeElementOf(element, to));
}

/**
 * Matches a string agains a regex and returns the match groups, or throws
 * and exception if it did not match. Useful for validating inputs.
 *
 * The seond parameter for the string input is curried for convenience.
 */
export function matchGroups(
  regexp: RegExp,
): ((input: string) => Record<string, string>) {
  return (input) => {
    const groups = input.match(regexp)?.groups;

    if (!groups) {
      throw new Error(`Did not match: ${input}`);
    }

    return groups;
  };
}

export function intersectSets<T>(...sets: Set<T>[]): Set<T>;
export function intersectSets<T>(
  set1: Iterable<T>,
  ...otherSets: Set<T>[]
): Set<T>;
export function intersectSets<T>(
  set1: Iterable<T>,
  ...otherSets: Set<T>[]
): Set<T> {
  return new Set(
    [...set1].filter((item) => otherSets.every((set) => set.has(item))),
  );
}

export function minusSets<T>(as: Iterable<T>, bs: Set<T>): Set<T> {
  return new Set([...as].filter((a) => !bs.has(a)));
}
