export const sum = (numbers: number[]): number =>
  numbers.reduce((acc, number) => acc + number, 0);

export const product = (numbers: number[]): number =>
  numbers.reduce((acc, number) => acc * number, 1);

export type Coord = readonly [number, number];

export const addCoords = (
  [x1, y1]: Coord,
  [x2, y2]: Coord,
): Coord => ([x1 + x2, y1 + y2]);

export const scaleCoord = ([x, y]: Coord, s: number): Coord => ([x * s, y * s]);

export const manhattanNormCoord = ([x, y]: Coord): number =>
  Math.abs(x) + Math.abs(y);

export const rotateLeftNinetyDegreesCoord = (
  [x, y]: Coord,
): Coord => [-y + 0, /* <- convert -0 to 0, lol */ x];

export type Maybe<T> = T | null;

export const maybeElementOf = <A, B extends A>(
  element: A,
  to: readonly B[],
): Maybe<B> => {
  if (!to.includes(element as B)) {
    return null;
  }

  return element as B;
};

export const assertMaybe = <T>(
  element: Maybe<T>,
): T => {
  if (element === null) {
    throw new Error(`Maybe: ${element}`);
  }

  return element;
};

export const ensureElementOf = <A, B extends A>(
  element: A,
  to: readonly B[],
): B => assertMaybe(maybeElementOf(element, to));

export const matchGroups = (regexp: RegExp) =>
  (input: string): Record<string, string> => {
    const groups = input.match(regexp)?.groups;

    if (!groups) throw new Error(`Did not match: ${input}`);

    return groups;
  };
