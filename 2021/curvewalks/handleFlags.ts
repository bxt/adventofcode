import { parseArgs } from "./deps.ts";

import * as curves from "./lindenmayerSystems.ts";

const specs = {
  recursion: "recursion must be an integer between 1 and 12",
  width: "width must be an integer > 0",
  height: "height must be an integer > 0",
  margin: "margin must be an integer >= 0",
  dotFraction: "dotFraction must be a number > 0",
  drawLine: "drawLine must be a flag",
  curve: `curve must be included in ${Object.keys(curves).join(", ")}`,
};

const descriptions = {
  recursion: "Depth of the fractal, higher means more iterations",
  width: "Final width of the image",
  height: "Final height of the image",
  margin: "Minimum space between image border and lines",
  dotFraction: "Fraction 1/n of a line occupied by a dot",
  drawLine: "If to draw the lines as well",
  curve: `Which curve to draw`,
};

const parseArgsOptions = {
  string: ["curve"],
  boolean: ["drawLine", "help"],
  alias: {
    "c": "curve",
    "h": "height",
    "l": "drawLine",
    "m": "margin",
    "r": "recursion",
    "s": "dotFraction",
    "w": "width",
  },
  default: {
    "dotFraction": 3,
    "height": 500,
    "margin": 20,
    "recursion": 5,
    "width": 500,
  },
  unknown: (option: string) => {
    console.error(`Ignoring unknown option: ${option}`);
    return false;
  },
};

export type HandleFlagsResult = { result: "help"; helpText: string } | {
  result: "error";
  error: string;
} | {
  result: "success";
  curve:
    | "gosper"
    | "boxy"
    | "peano"
    | "hilbert"
    | "kochSnowflake"
    | "sierpinski"
    | "sierpinskiArrowhead";
  dotFraction: number;
  drawLine: boolean;
  height: number;
  margin: number;
  recursion: number;
  width: number;
};

export function handleFlags(flags: string[]): HandleFlagsResult {
  const {
    curve,
    dotFraction,
    drawLine,
    height,
    help,
    margin,
    recursion,
    width,
  } = parseArgs(flags, parseArgsOptions);

  if (help) {
    let helpText = "Renders some nice curves\n\nOptions:\n\n";

    for (const [short, long] of Object.entries(parseArgsOptions.alias)) {
      const def = parseArgsOptions
        .default[long as keyof typeof parseArgsOptions.default];
      const spec = specs[long as keyof typeof specs];

      helpText += `  -${short}, --${long}:\n`;
      helpText += `    ${descriptions[long as keyof typeof descriptions]}`;

      if (spec !== undefined) helpText += `, ${spec}. `;
      else helpText += `. `;

      if (def !== undefined) helpText += `Default: ${def}.`;

      helpText += "\n\n";
    }

    return { result: "help", helpText };
  }

  function ensureInteger(input: unknown): input is number {
    return Math.round(input as number) === input;
  }

  if (!ensureInteger(recursion) || recursion < 1 || recursion > 12) {
    return {
      result: "error",
      error: "recursion must be an integer between 1 and 12",
    };
  }

  if (!ensureInteger(width) || width < 1) {
    return { result: "error", error: "width must be an integer > 0" };
  }

  if (!ensureInteger(height) || height < 1) {
    return { result: "error", error: "height must be an integer > 0" };
  }

  if (!ensureInteger(margin) || margin < 0) {
    return { result: "error", error: "margin must be an integer >= 0" };
  }
  if (margin * 2 > width || margin * 2 > height) {
    return { result: "error", error: "margin must fit into width and height" };
  }

  if (typeof dotFraction !== "number" || dotFraction <= 0) {
    return { result: "error", error: "dotFraction must be a number > 0" };
  }

  if (typeof drawLine !== "boolean") {
    return { result: "error", error: "drawLine must be a flag" };
  }

  // deno-lint-ignore no-explicit-any
  function isValidCurve(value: any): value is keyof typeof curves {
    return value in curves;
  }
  if (!isValidCurve(curve)) {
    return {
      result: "error",
      error: `curve must be included in ${Object.keys(curves).join(", ")}`,
    };
  }

  return {
    result: "success",
    curve,
    dotFraction,
    drawLine,
    height,
    margin,
    recursion,
    width,
  };
}
