import { addCoords, Coord, range, scaleCoord } from "./deps.ts";
import { LSystem } from "./lindenmayerSystems.ts";

export function getSymbols(
  { start, productions }: LSystem,
  stages: number,
): string {
  let symbols = start;
  range(stages).forEach(() => {
    symbols = symbols.replaceAll(/./g, (match) => productions[match] ?? match);
  });
  return symbols;
}

type Instruction =
  | { type: "forward"; length: number }
  | { type: "rotate"; angle: number };

export function mapSymbols(symbols: string, angle: number): Instruction[] {
  return [...symbols.matchAll(/-|\+|[A-Z]/g)].map(([match]) => {
    if (match === "-") {
      return { type: "rotate", angle: -angle };
    } else if (match === "+") {
      return { type: "rotate", angle };
    } else {
      return { type: "forward", length: 1 };
    }
  });
}

function getLSystemInstructions(lSystem: LSystem, stages: number) {
  const symbols = getSymbols(lSystem, stages);
  return mapSymbols(symbols, lSystem.angle);
}

function rotateCoord([x, y]: Coord, angle: number): Coord {
  return [
    x * Math.cos(angle) - y * Math.sin(angle),
    x * Math.sin(angle) + y * Math.cos(angle),
  ];
}

export function walkInstructions(instructions: Instruction[]): Coord[] {
  let positon: Coord = [0, 0];
  let direction: Coord = [1, 0];
  const points = [positon];
  for (const instruction of instructions) {
    if (instruction.type === "rotate") {
      direction = rotateCoord(direction, instruction.angle);
    } else if (instruction.type === "forward") {
      positon = addCoords(positon, scaleCoord(direction, instruction.length));
      points.push(positon);
    }
  }
  return points;
}

export function getCurvePoints(system: LSystem, stages: number): Coord[] {
  return walkInstructions(
    getLSystemInstructions(system, stages),
  );
}
