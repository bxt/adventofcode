import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";

/**
 * 2020 TypeScript Utilities
 *
 * This contains a bunch of utilities I used for solving the 2020 riddles.
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
  return [x1 + x2, y1 + y2];
}

/**
 * Scales a coord with a scalar (plain number), multiplies each component.
 */
export function scaleCoord([x, y]: Coord, s: number): Coord {
  return [x * s, y * s];
}

/**
 * Calculates the manhattan distance form the coord to the origin `(0,0)`.
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
 * Finds the element at the specified coords in a
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
 * Can contain a value or not, used to make assertMaybe more semantic.
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
 * Matches a string against a regex and returns the match groups, or throws
 * and exception if it did not match. Useful for validating inputs.
 *
 * The second parameter for the string input is curried for convenience.
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
/**
 * Calculates the intersection (∩) of a bunch of sets, that is creates a new
 * set with the elements that are in every one of the input sets.
 */
export function intersectSets<T>(
  set1: Iterable<T>,
  ...otherSets: Set<T>[]
): Set<T> {
  return new Set(
    [...set1].filter((item) => otherSets.every((set) => set.has(item))),
  );
}

/**
 * Calculates the set difference (∖, relative complement) of two sets, that
 * is creates a new set with the elements of the first set without those
 * form the second set.
 */
export function minusSets<T>(as: Iterable<T>, bs: Set<T>): Set<T> {
  return new Set([...as].filter((a) => !bs.has(a)));
}

/**
 * Allows to build a `Set` of arbitrary objects by converting them to a string
 * representation. Overwrite the `stringify` method in a subclass to make it
 * work for your type, e.g. `JSON.stringify` can be used if you know that key
 * order does not change. The result of `stringify` should not change for an
 * object, so it works best with `readonly` objects. Implements all the set
 * methods that do not rely on taking the objects out of the set again, if you
 * need to read the objects back use `StringifySet` which implements all set
 * methods.
 */
export abstract class StringifySinkSet<T> {
  protected set: Set<string>;

  constructor(
    elements?: Iterable<T>,
  ) {
    if (elements) {
      this.set = new Set([...elements].map(this.stringify));
    } else {
      this.set = new Set();
    }
  }

  get [Symbol.toStringTag](): string {
    return "StringifySet";
  }

  add(element: T): this {
    this.set.add(this.stringify(element));
    return this;
  }

  has(element: T): boolean {
    return this.set.has(this.stringify(element));
  }

  delete(element: T): boolean {
    return this.set.delete(this.stringify(element));
  }

  get size(): number {
    return this.set.size;
  }

  clear(): void {
    this.set.clear();
  }

  protected abstract stringify(element: T): string;
}

/**
 * Allows to build a `Set` of arbitrary objects by converting them to a string
 * representation and back. Overwrite the `parse` method in a subclass to make it
 * work for your type, e.g. `JSON.parse` can be used if `stringify` was also
 * implemented accordingly. This extends `StringifySinkSet` adding the `Set`
 * methods for reading the elements of the set, so that it implements the whole
 * `Set` interface.
 */
export abstract class StringifySet<T> extends StringifySinkSet<T>
  implements Set<T> {
  *[Symbol.iterator](): IterableIterator<T> {
    for (const stringItem of this.set) {
      yield this.parse(stringItem);
    }
  }

  forEach(callback: (element: T, element2: T, set: Set<T>) => void) {
    for (const item of this) {
      callback(item, item, this);
    }
  }

  *entries(): IterableIterator<[T, T]> {
    for (const stringItem of this.set) {
      const item = this.parse(stringItem);
      yield [item, item];
    }
  }

  keys(): IterableIterator<T> {
    return this[Symbol.iterator]();
  }

  values(): IterableIterator<T> {
    return this[Symbol.iterator]();
  }

  protected abstract parse(string: string): T;
}

/**
 * A set of coordinates. Uses a normal `Set` as a base and converts the
 * coordinates to and from strings using `StringifySet` to make it work.
 */
export class CoordSet extends StringifySet<Coord> {
  protected stringify(element: Coord): string {
    return element.join(",");
  }
  protected parse(string: string): Coord {
    const components = string.split(",");
    assertEquals(components.length, 2);
    return components.map(Number) as unknown as Coord;
  }
}
