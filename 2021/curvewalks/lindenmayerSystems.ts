export type LSystem = {
  start: string;
  productions: Record<string, string>;
  angle: number;
};

export const gosper: LSystem = {
  start: "A",
  productions: { A: "A-B--B+A++AA+B-", B: "+A-BB--B-A++A+B" },
  angle: Math.PI / 3,
};

export const boxy: LSystem = {
  start: "F+F+F+F", // a suqare
  productions: { F: "F+F-F-FF+F+F-F" },
  angle: Math.PI / 2,
};

// The lower case letters are just for replacement and are filtered out then
export const peano: LSystem = {
  start: "a",
  productions: {
    a: "aFbFa+F+bFaFb-F-aFbFa",
    b: "bFaFb-F-aFbFa+F+bFaFb",
  },
  angle: Math.PI / 2,
};

export const hilbert: LSystem = {
  start: "a",
  productions: {
    a: "-bF+aFa+Fb-",
    b: "+aF-bFb-Fa+",
  },
  angle: Math.PI / 2,
};

export const kochSnowflake: LSystem = {
  start: "F--F--F", // equilateral triangle
  productions: { F: "F+F--F+F" },
  angle: Math.PI / 3,
};

export const sierpinski: LSystem = {
  start: "F+F+F+F+F+F",
  productions: { F: "F-F+F+F+F-F" },
  angle: Math.PI / 3,
};

export const sierpinskiArrowhead: LSystem = {
  start: "aF",
  productions: {
    a: "bF-aF-b",
    b: "aF+bF+a",
  },
  angle: Math.PI / 3,
};
