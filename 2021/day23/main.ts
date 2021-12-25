#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { ensureElementOf, matchGroups, range } from "../../2020/utils.ts";

const amphipods = ["A", "B", "C", "D"] as const;
type Amphipod = typeof amphipods[number];

type Burrow = {
  hallwaySpots: (Amphipod | undefined)[];
  sideRooms: Amphipod[][];
};

// #############
// #01.2.3.4.56#
// ###0#1#2#3###
//   #.#.#.#.#
//   #########

type MoveToHallway = {
  type: "toHallway";
  fromSideRoom: number;
  toHallwaySpot: number;
};

type MoveToSideRoom = {
  type: "toSideRoom";
  fromHallwaySpot: number;
  toSideRoom: number;
};

type Move = MoveToSideRoom | MoveToHallway;

const matchInput = matchGroups(
  /#(?<c1>[A-D])#(?<c2>[A-D])#(?<c3>[A-D])#(?<c4>[A-D])#/,
);

function parseLine(line: string): Amphipod[] {
  const { c1, c2, c3, c4 } = matchInput(line);
  return [c1, c2, c3, c4].map((s) => ensureElementOf(s, amphipods));
}

function parseInput(string: string): Burrow {
  const lines = string.trim().split("\n");
  const [, , line1, line2] = lines;
  const sideRoomConfig = [line1, "#D#C#B#A#", "#D#B#A#C#", line2].map(
    parseLine,
  );

  const initialBurrow = {
    hallwaySpots: range(7).map(() => undefined),
    sideRooms: range(4).map((_, i) => sideRoomConfig.map((a) => a[i])),
  };

  return initialBurrow;
}

const example = parseInput(`
  #############
  #...........#
  ###B#C#B#D###
    #A#D#C#A#
    #########
`);

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

function isPathFree(
  { hallwaySpot, sideRoom, burrow: { hallwaySpots } }: {
    hallwaySpot: number;
    sideRoom: number;
    burrow: Burrow;
  },
): boolean {
  const leftHallway = sideRoom + 1;
  const rightHallway = sideRoom + 2;
  if (hallwaySpot < leftHallway) {
    for (let i = hallwaySpot + 1; i <= leftHallway; i++) {
      if (hallwaySpots[i] !== undefined) return false;
    }
    return true;
  } else if (hallwaySpot > rightHallway) {
    for (let i = rightHallway; i < hallwaySpot; i++) {
      if (hallwaySpots[i] !== undefined) return false;
    }
    return true;
  } else {
    return true;
  }
}

function assertFreePaths(burrow: Burrow, expectedValues: string) {
  range(7).forEach((hallwaySpot) =>
    range(4).forEach((sideRoom) => {
      const expected =
        expectedValues.charAt(sideRoom * 8 + hallwaySpot) === "y";
      assertEquals(
        isPathFree({ burrow, hallwaySpot, sideRoom }),
        expected,
        `hallwaySpot ${hallwaySpot}, sideRoom ${sideRoom} not ${expected}`,
      );
    })
  );
}

assertFreePaths({
  sideRooms: [],
  hallwaySpots: [
    undefined,
    undefined,
    undefined,
    undefined,
    undefined,
    undefined,
    undefined,
  ],
}, "yyyyyyy yyyyyyy yyyyyyy yyyyyyy");

assertFreePaths({
  sideRooms: [],
  hallwaySpots: [
    "A",
    undefined,
    undefined,
    undefined,
    undefined,
    undefined,
    undefined,
  ],
}, "yyyyyyy yyyyyyy yyyyyyy yyyyyyy");

assertFreePaths({
  sideRooms: [],
  hallwaySpots: [
    undefined,
    "A",
    undefined,
    undefined,
    undefined,
    undefined,
    undefined,
  ],
}, "nyyyyyy nyyyyyy nyyyyyy nyyyyyy");

assertFreePaths({
  sideRooms: [],
  hallwaySpots: [
    undefined,
    undefined,
    "A",
    undefined,
    undefined,
    undefined,
    undefined,
  ],
}, "yyynnnn nnyyyyy nnyyyyy nnyyyyy");

assertFreePaths({
  sideRooms: [],
  hallwaySpots: [
    undefined,
    undefined,
    undefined,
    "A",
    undefined,
    undefined,
    undefined,
  ],
}, "yyyynnn yyyynnn nnnyyyy nnnyyyy");

assertFreePaths({
  sideRooms: [],
  hallwaySpots: [
    undefined,
    undefined,
    undefined,
    undefined,
    "A",
    undefined,
    undefined,
  ],
}, "yyyyynn yyyyynn yyyyynn nnnnyyy");

assertFreePaths({
  sideRooms: [],
  hallwaySpots: [
    undefined,
    undefined,
    "A",
    undefined,
    "A",
    undefined,
    undefined,
  ],
}, "yyynnnn nnyyynn nnyyynn nnnnyyy");

function getPossibleMoves(burrow: Burrow): Move[] {
  const { hallwaySpots, sideRooms } = burrow;

  const movesToSideRoom = range(7).flatMap((fromHallwaySpot) =>
    range(4).map((toSideRoom) => ({
      fromHallwaySpot,
      toSideRoom,
      type: "toSideRoom" as const,
    }))
  ).filter(({ fromHallwaySpot, toSideRoom }) => {
    const amphipod = hallwaySpots[fromHallwaySpot];
    if (amphipod === undefined) return false;
    if (sideRooms[toSideRoom].length >= 4) return false;
    if (amphipods.indexOf(amphipod) !== toSideRoom) return false;
    if (sideRooms[toSideRoom].some((a) => a !== amphipod)) return false;
    if (
      !isPathFree({
        burrow,
        hallwaySpot: fromHallwaySpot,
        sideRoom: toSideRoom,
      })
    ) {
      return false;
    }
    return true;
  });

  if (movesToSideRoom.length > 0) return movesToSideRoom;

  const movesToHallway = range(4).flatMap((fromSideRoom) =>
    range(7).map((toHallwaySpot) => ({
      fromSideRoom,
      toHallwaySpot,
      type: "toHallway" as const,
    }))
  ).filter(({ fromSideRoom, toHallwaySpot }) => {
    const amphipod = sideRooms[fromSideRoom][0];
    if (amphipod === undefined) return false;
    if (hallwaySpots[toHallwaySpot] !== undefined) return false;
    if (
      amphipods.indexOf(amphipod) === fromSideRoom &&
      sideRooms[fromSideRoom].every((a) => a === amphipod)
    ) {
      return false;
    }
    if (
      !isPathFree({
        burrow,
        hallwaySpot: toHallwaySpot,
        sideRoom: fromSideRoom,
      })
    ) {
      return false;
    }
    return true;
  });

  return movesToHallway;
}

assertEquals(
  getPossibleMoves({
    hallwaySpots: [
      "C",
      undefined,
      undefined,
      undefined,
      undefined,
      undefined,
      "A",
    ],
    sideRooms: [
      ["A", "A", "A"],
      ["B", "B", "B", "B"],
      ["C", "C", "C"],
      ["D", "D", "D", "D"],
    ],
  }),
  [
    { fromHallwaySpot: 0, toSideRoom: 2, type: "toSideRoom" },
    { fromHallwaySpot: 6, toSideRoom: 0, type: "toSideRoom" },
  ],
);

assertEquals(
  getPossibleMoves({
    hallwaySpots: [
      "C",
      "A",
      undefined,
      undefined,
      undefined,
      undefined,
      undefined,
    ],
    sideRooms: [
      ["A", "A", "A"],
      ["B", "B", "B", "B"],
      ["C", "C", "C"],
      ["D", "D", "D", "D"],
    ],
  }),
  [{ fromHallwaySpot: 1, toSideRoom: 0, type: "toSideRoom" }],
);

function cloneBurrow(burrow: Burrow): Burrow {
  return {
    hallwaySpots: [...burrow.hallwaySpots],
    sideRooms: [...burrow.sideRooms.map((s) => [...s])],
  };
}

function executeMove(burrow: Burrow, move: Move): Burrow {
  const newBurrow = cloneBurrow(burrow);
  if (move.type === "toHallway") {
    const { fromSideRoom, toHallwaySpot } = move;
    const amphipod = newBurrow.sideRooms[fromSideRoom].shift();
    newBurrow.hallwaySpots[toHallwaySpot] = amphipod;
    return newBurrow;
  } else {
    const { fromHallwaySpot, toSideRoom } = move;
    const amphipod = newBurrow.hallwaySpots[fromHallwaySpot];
    newBurrow.hallwaySpots[fromHallwaySpot] = undefined;
    if (amphipod === undefined) throw new Error(":/");
    newBurrow.sideRooms[toSideRoom].unshift(amphipod);
    return newBurrow;
  }
}

function amphipodCostFactor(amphipod: Amphipod): number {
  return 10 ** amphipods.indexOf(amphipod);
}

assertEquals(amphipodCostFactor("A"), 1);
assertEquals(amphipodCostFactor("B"), 10);
assertEquals(amphipodCostFactor("C"), 100);
assertEquals(amphipodCostFactor("D"), 1000);

function stepsBetweenTopOfSideRoomToHallway(
  { sideRoom, hallwaySpot }: { sideRoom: number; hallwaySpot: number },
): number {
  const toClosestHallway = 1;

  const leftHallway = sideRoom + 1;
  const rightHallway = sideRoom + 2;
  let horizontal;
  if (hallwaySpot < leftHallway) {
    horizontal = (leftHallway - hallwaySpot) * 2;
  } else if (hallwaySpot > rightHallway) {
    horizontal = (hallwaySpot - rightHallway) * 2;
  } else {
    horizontal = 0;
  }
  const horizontalCorrection = Number(
    hallwaySpot === 0 || hallwaySpot === 6,
  );

  return toClosestHallway + horizontal - horizontalCorrection;
}

function calculateMoveCost(burrow: Burrow, move: Move): number {
  if (move.type === "toHallway") {
    const { fromSideRoom, toHallwaySpot } = move;
    const amphipod = burrow.sideRooms[fromSideRoom][0];
    const outOfSideRoom = 4 - burrow.sideRooms[fromSideRoom].length + 1;
    const horizontal = stepsBetweenTopOfSideRoomToHallway({
      sideRoom: fromSideRoom,
      hallwaySpot: toHallwaySpot,
    });
    return amphipodCostFactor(amphipod) * (outOfSideRoom + horizontal);
  } else {
    const { fromHallwaySpot, toSideRoom } = move;
    const amphipod = burrow.hallwaySpots[fromHallwaySpot];
    if (amphipod === undefined) throw new Error(":/");
    const intoSideRoom = 4 - burrow.sideRooms[toSideRoom].length;
    const horizontal = stepsBetweenTopOfSideRoomToHallway({
      sideRoom: toSideRoom,
      hallwaySpot: fromHallwaySpot,
    });
    return amphipodCostFactor(amphipod) * (horizontal + intoSideRoom);
  }
}

assertEquals(
  calculateMoveCost({
    sideRooms: [
      ["A", "A", "A", "A"],
      ["B", "B", "B", "B"],
      ["C", "C", "C", "C"],
      ["D", "D", "D"],
    ],
    hallwaySpots: [
      undefined,
      undefined,
      undefined,
      undefined,
      undefined,
      undefined,
      "D",
    ],
  }, { fromHallwaySpot: 6, toSideRoom: 3, type: "toSideRoom" }),
  3000,
);

{
  const burrow: Burrow = {
    sideRooms: [
      ["A", "A", "A", "A"],
      ["B", "B", "B"],
      ["C", "C", "C", "C"],
      ["B", "D", "D"],
    ],
    hallwaySpots: [
      undefined,
      undefined,
      undefined,
      undefined,
      undefined,
      "D",
      "D",
    ],
  };
  const costs = [100, 90, 70, 50, 30, 30, 40];
  for (let toHallwaySpot = 5; toHallwaySpot < costs.length; toHallwaySpot++) {
    assertEquals(
      calculateMoveCost(burrow, {
        fromSideRoom: 3,
        toHallwaySpot,
        type: "toHallway",
      }),
      costs[toHallwaySpot],
    );
  }
}

function isFinished(burrow: Burrow): boolean {
  return amphipods.every((amphipod, index) =>
    burrow.sideRooms[index].length === 4 &&
    burrow.sideRooms[index].every((a) => a === amphipod)
  );
}

function getBestFinishingMoves(
  burrow: Burrow,
): [number, Move[] | undefined] {
  let bestMoves = undefined;
  let bestResult = Infinity;
  const stack: [Move[], Burrow, number][] = [[[], burrow, 0]];
  while (stack.length > 0) {
    const top = stack.pop();
    if (top === undefined) throw new Error("Stack somehow got empty!?");
    const [moves, burrow, result] = top;

    if (result > bestResult) continue;

    const possibleMoves = getPossibleMoves(burrow);
    if (possibleMoves.length === 0) {
      if (isFinished(burrow)) {
        if (result < bestResult) {
          bestResult = result;
          bestMoves = moves;
        }
      }
    } else {
      possibleMoves.forEach((move) => {
        const newMoves = [...moves, move];
        const newBurrow = executeMove(burrow, move);
        const newResult = result + calculateMoveCost(burrow, move);
        stack.push([newMoves, newBurrow, newResult]);
      });
    }
  }
  return [bestResult, bestMoves];
}

assertEquals(
  getBestFinishingMoves({
    sideRooms: [
      ["A", "A", "A", "A"],
      ["B", "B", "B", "B"],
      ["C", "C", "C", "C"],
      ["D", "D", "D"],
    ],
    hallwaySpots: [
      undefined,
      undefined,
      undefined,
      undefined,
      undefined,
      undefined,
      "D",
    ],
  })[1],
  [
    { fromHallwaySpot: 6, toSideRoom: 3, type: "toSideRoom" },
  ],
);

assertEquals(getBestFinishingMoves(example)[0], 44169);

function stringifyBurrow(burrow: Burrow) {
  const hs = burrow.hallwaySpots.map((h) => h ?? ".");
  const sr = burrow.sideRooms.map((s) => s.join("").padStart(4, ".").split(""));
  return "" +
    "#############\n" +
    `#${hs[0]}${hs[1]}.${hs[2]}.${hs[3]}.${hs[4]}.${hs[5]}${hs[6]}#\n` +
    `###${sr[0][0]}#${sr[1][0]}#${sr[2][0]}#${sr[3][0]}###\n` +
    `  #${sr[0][1]}#${sr[1][1]}#${sr[2][1]}#${sr[3][1]}#\n` +
    `  #${sr[0][2]}#${sr[1][2]}#${sr[2][2]}#${sr[3][2]}#\n` +
    `  #${sr[0][3]}#${sr[1][3]}#${sr[2][3]}#${sr[3][3]}#\n` +
    "  #########\n\n";
}

function part2(input: Burrow): number {
  const [result, moves] = getBestFinishingMoves(input);
  if (moves === undefined) throw new Error();

  console.log(result);

  let burrow = input;
  for (const move of moves) {
    console.log(stringifyBurrow(burrow));
    console.log(move);
    console.log(`+ ${calculateMoveCost(burrow, move)} cost`);
    burrow = executeMove(burrow, move);
  }
  console.log(stringifyBurrow(burrow));

  return result;
}

console.log("Result part 2: " + part2(input));
