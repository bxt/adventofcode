#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { matchGroups, sum } from "../../2020/utils.ts";

const matchInput = matchGroups(
  /(?<state>on|off) x=(?<x1>-?\d+)..(?<x2>-?\d+),y=(?<y1>-?\d+)..(?<y2>-?\d+),z=(?<z1>-?\d+)..(?<z2>-?\d+)/,
);

type Cuboid = {
  state: boolean;
  x1: number;
  x2: number;
  y1: number;
  y2: number;
  z1: number;
  z2: number;
};

function parseInput(string: string): Cuboid[] {
  const lines = string.trim().split("\n");
  return lines.map((line) => {
    const { state, x1, x2, y1, y2, z1, z2 } = matchInput(line);
    return {
      state: state === "on",
      x1: parseInt(x1, 10),
      x2: parseInt(x2, 10),
      y1: parseInt(y1, 10),
      y2: parseInt(y2, 10),
      z1: parseInt(z1, 10),
      z2: parseInt(z2, 10),
    };
  });
}

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

function part1(input: Cuboid[]): number {
  const cubeStates: Record<string, boolean> = {};

  for (const cuboid of input) {
    const { state, x1, x2, y1, y2, z1, z2 } = cuboid;
    for (let x = Math.max(x1, -50); x <= Math.min(x2, 50); x++) {
      for (let y = Math.max(y1, -50); y <= Math.min(y2, 50); y++) {
        for (let z = Math.max(z1, -50); z <= Math.min(z2, 50); z++) {
          cubeStates[`${x},${y},${z}`] = state;
        }
      }
    }
  }

  return Object.values(cubeStates).filter((b) => b).length;
}

const example = parseInput(`
  on x=10..12,y=10..12,z=10..12
  on x=11..13,y=11..13,z=11..13
  off x=9..11,y=9..11,z=9..11
  on x=10..10,y=10..10,z=10..10
`);

assertEquals(part1(example), 39);

console.log("Result part 1: " + part1(input));

function cuboidSize({ x1, x2, y1, y2, z1, z2 }: Cuboid) {
  return (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1);
}

assertEquals(example.map(cuboidSize), [27, 27, 27, 1]);

type Interval = [number, number];

type Overlap = { me: Interval[]; both: Interval[]; other: Interval[] };

function filterEmptyIntervals(intervals: Interval[]): Interval[] {
  return intervals.filter(([from, to]) => from <= to);
}

function equals(
  [from, to]: Interval,
  [otherFrom, otherTo]: Interval,
): boolean {
  return from === otherFrom && to === otherTo;
}

function overlap(
  [from, to]: Interval,
  [otherFrom, otherTo]: Interval,
): Overlap {
  if (from < otherFrom) {
    if (to < otherFrom) {
      // me:    |-----|
      // other:         |------|
      return {
        me: [[from, to]],
        other: [[otherFrom, otherTo]],
        both: [],
      };
    } else {
      if (to < otherTo) {
        // me:    |-----|
        // other:     |------|
        return {
          me: filterEmptyIntervals([[from, otherFrom - 1]]),
          other: filterEmptyIntervals([[to + 1, otherTo]]),
          both: filterEmptyIntervals([[otherFrom, to]]),
        };
      } else {
        // me:    |-------|
        // other:   |--|
        return {
          me: filterEmptyIntervals([[from, otherFrom - 1], [otherTo + 1, to]]),
          other: [],
          both: [[otherFrom, otherTo]],
        };
      }
    }
  } else {
    if (from > otherTo) {
      // me:              |-----|
      // other: |------|
      return {
        me: [[from, to]],
        other: [[otherFrom, otherTo]],
        both: [],
      };
    } else {
      if (to > otherTo) {
        // me:        |-----|
        // other: |------|
        return {
          me: filterEmptyIntervals([[otherTo + 1, to]]),
          other: filterEmptyIntervals([[otherFrom, from - 1]]),
          both: filterEmptyIntervals([[from, otherTo]]),
        };
      } else {
        // me:       |---|
        // other:  |--------|
        return {
          me: [],
          other: filterEmptyIntervals([[otherFrom, from - 1], [
            to + 1,
            otherTo,
          ]]),
          both: [[from, to]],
        };
      }
    }
  }
}

assertEquals(overlap([2, 4], [5, 7]), {
  me: [[2, 4]],
  other: [[5, 7]],
  both: [],
});
assertEquals(overlap([2, 5], [5, 7]), {
  me: [[2, 4]],
  other: [[6, 7]],
  both: [[5, 5]],
});
assertEquals(overlap([2, 6], [5, 7]), {
  me: [[2, 4]],
  other: [[7, 7]],
  both: [[5, 6]],
});
assertEquals(overlap([2, 7], [5, 7]), {
  me: [[2, 4]],
  other: [],
  both: [[5, 7]],
});
assertEquals(overlap([2, 8], [5, 7]), {
  me: [[2, 4], [8, 8]],
  other: [],
  both: [[5, 7]],
});
assertEquals(overlap([5, 8], [5, 7]), {
  me: [[8, 8]],
  other: [],
  both: [[5, 7]],
});
assertEquals(overlap([5, 8], [5, 8]), {
  me: [],
  other: [],
  both: [[5, 8]],
});
assertEquals(overlap([8, 9], [5, 7]), {
  me: [[8, 9]],
  other: [[5, 7]],
  both: [],
});
assertEquals(overlap([8, 9], [5, 8]), {
  me: [[9, 9]],
  other: [[5, 7]],
  both: [[8, 8]],
});
assertEquals(overlap([9, 9], [5, 8]), {
  me: [[9, 9]],
  other: [[5, 8]],
  both: [],
});
assertEquals(overlap([8, 9], [5, 11]), {
  me: [],
  other: [[5, 7], [10, 11]],
  both: [[8, 9]],
});
assertEquals(overlap([8, 9], [8, 11]), {
  me: [],
  other: [[10, 11]],
  both: [[8, 9]],
});
assertEquals(overlap([8, 9], [5, 9]), {
  me: [],
  other: [[5, 7]],
  both: [[8, 9]],
});

function cutOutCuboids(cuboid: Cuboid, otherCuboid: Cuboid) {
  const { x1, x2, y1, y2, z1, z2 } = cuboid;
  const {
    state,
    x1: ox1,
    x2: ox2,
    y1: oy1,
    y2: oy2,
    z1: oz1,
    z2: oz2,
  } = otherCuboid;

  const overlapX = overlap([x1, x2], [ox1, ox2]);
  const overlapY = overlap([y1, y2], [oy1, oy2]);
  const overlapZ = overlap([z1, z2], [oz1, oz2]);
  const hasOverlapX = overlapX.both.length >= 1;
  const hasOverlapY = overlapY.both.length >= 1;
  const hasOverlapZ = overlapZ.both.length >= 1;

  if (hasOverlapX && hasOverlapY && hasOverlapZ) {
    const newCuboids: Cuboid[] = [];
    for (const xInterval of [...overlapX.other, ...overlapX.both]) {
      for (const yInterval of [...overlapY.other, ...overlapY.both]) {
        for (const zInterval of [...overlapZ.other, ...overlapZ.both]) {
          if (
            equals(xInterval, overlapX.both[0]) &&
            equals(yInterval, overlapY.both[0]) &&
            equals(zInterval, overlapZ.both[0])
          ) {
            continue;
          }
          const [x1, x2] = xInterval;
          const [y1, y2] = yInterval;
          const [z1, z2] = zInterval;
          newCuboids.push({ x1, x2, y1, y2, z1, z2, state });
        }
      }
    }
    return newCuboids;
  } else {
    return otherCuboid;
  }
}

assertEquals(
  cutOutCuboids(
    { x1: 3, x2: 5, y1: 7, y2: 9, z1: 12, z2: 13, state: true },
    { x1: 4, x2: 6, y1: 8, y2: 10, z1: 13, z2: 13, state: false },
  ),
  [
    { state: false, x1: 6, x2: 6, y1: 10, y2: 10, z1: 13, z2: 13 },
    { state: false, x1: 6, x2: 6, y1: 8, y2: 9, z1: 13, z2: 13 },
    { state: false, x1: 4, x2: 5, y1: 10, y2: 10, z1: 13, z2: 13 },
  ],
);

function part2(input: Cuboid[]): number {
  const [firstCuboid, ...inputCuboids] = input;
  let cuboids: Cuboid[] = [firstCuboid];

  for (const cuboid of inputCuboids) {
    cuboids = [
      cuboid,
      ...cuboids.flatMap((otherCuboid) => cutOutCuboids(cuboid, otherCuboid)),
    ];
  }

  return sum(
    Object.values(cuboids).filter(({ state }) => state).map(cuboidSize),
  );
}

const example2 = parseInput(`
  on x=-5..47,y=-31..22,z=-19..33
  on x=-44..5,y=-27..21,z=-14..35
  on x=-49..-1,y=-11..42,z=-10..38
  on x=-20..34,y=-40..6,z=-44..1
  off x=26..39,y=40..50,z=-2..11
  on x=-41..5,y=-41..6,z=-36..8
  off x=-43..-33,y=-45..-28,z=7..25
  on x=-33..15,y=-32..19,z=-34..11
  off x=35..47,y=-46..-34,z=-11..5
  on x=-14..36,y=-6..44,z=-16..29
  on x=-57795..-6158,y=29564..72030,z=20435..90618
  on x=36731..105352,y=-21140..28532,z=16094..90401
  on x=30999..107136,y=-53464..15513,z=8553..71215
  on x=13528..83982,y=-99403..-27377,z=-24141..23996
  on x=-72682..-12347,y=18159..111354,z=7391..80950
  on x=-1060..80757,y=-65301..-20884,z=-103788..-16709
  on x=-83015..-9461,y=-72160..-8347,z=-81239..-26856
  on x=-52752..22273,y=-49450..9096,z=54442..119054
  on x=-29982..40483,y=-108474..-28371,z=-24328..38471
  on x=-4958..62750,y=40422..118853,z=-7672..65583
  on x=55694..108686,y=-43367..46958,z=-26781..48729
  on x=-98497..-18186,y=-63569..3412,z=1232..88485
  on x=-726..56291,y=-62629..13224,z=18033..85226
  on x=-110886..-34664,y=-81338..-8658,z=8914..63723
  on x=-55829..24974,y=-16897..54165,z=-121762..-28058
  on x=-65152..-11147,y=22489..91432,z=-58782..1780
  on x=-120100..-32970,y=-46592..27473,z=-11695..61039
  on x=-18631..37533,y=-124565..-50804,z=-35667..28308
  on x=-57817..18248,y=49321..117703,z=5745..55881
  on x=14781..98692,y=-1341..70827,z=15753..70151
  on x=-34419..55919,y=-19626..40991,z=39015..114138
  on x=-60785..11593,y=-56135..2999,z=-95368..-26915
  on x=-32178..58085,y=17647..101866,z=-91405..-8878
  on x=-53655..12091,y=50097..105568,z=-75335..-4862
  on x=-111166..-40997,y=-71714..2688,z=5609..50954
  on x=-16602..70118,y=-98693..-44401,z=5197..76897
  on x=16383..101554,y=4615..83635,z=-44907..18747
  off x=-95822..-15171,y=-19987..48940,z=10804..104439
  on x=-89813..-14614,y=16069..88491,z=-3297..45228
  on x=41075..99376,y=-20427..49978,z=-52012..13762
  on x=-21330..50085,y=-17944..62733,z=-112280..-30197
  on x=-16478..35915,y=36008..118594,z=-7885..47086
  off x=-98156..-27851,y=-49952..43171,z=-99005..-8456
  off x=2032..69770,y=-71013..4824,z=7471..94418
  on x=43670..120875,y=-42068..12382,z=-24787..38892
  off x=37514..111226,y=-45862..25743,z=-16714..54663
  off x=25699..97951,y=-30668..59918,z=-15349..69697
  off x=-44271..17935,y=-9516..60759,z=49131..112598
  on x=-61695..-5813,y=40978..94975,z=8655..80240
  off x=-101086..-9439,y=-7088..67543,z=33935..83858
  off x=18020..114017,y=-48931..32606,z=21474..89843
  off x=-77139..10506,y=-89994..-18797,z=-80..59318
  off x=8476..79288,y=-75520..11602,z=-96624..-24783
  on x=-47488..-1262,y=24338..100707,z=16292..72967
  off x=-84341..13987,y=2429..92914,z=-90671..-1318
  off x=-37810..49457,y=-71013..-7894,z=-105357..-13188
  off x=-27365..46395,y=31009..98017,z=15428..76570
  off x=-70369..-16548,y=22648..78696,z=-1892..86821
  on x=-53470..21291,y=-120233..-33476,z=-44150..38147
  off x=-93533..-4276,y=-16170..68771,z=-104985..-24507
`);

assertEquals(part2(example2), 2758514936282235);

console.log("Result part 2: " + part2(input));
