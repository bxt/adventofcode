#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";

type Coord = [number, number, number];
type Scanner = Coord[];

const parseInput = (string: string): Scanner[] => {
  return string.trim().split(/\n\s*\n/).map((scannerString) => {
    return scannerString.trim().split("\n").slice(1).map((line) => {
      const [x, y, z] = line.trim().split(",").map((s) => parseInt(s, 10));
      return [x, y, z];
    });
  });
};

const text = await Deno.readTextFile("input.txt");

export const input = parseInput(text);

const example = parseInput(`
  --- scanner 0 ---
  404,-588,-901
  528,-643,409
  -838,591,734
  390,-675,-793
  -537,-823,-458
  -485,-357,347
  -345,-311,381
  -661,-816,-575
  -876,649,763
  -618,-824,-621
  553,345,-567
  474,580,667
  -447,-329,318
  -584,868,-557
  544,-627,-890
  564,392,-477
  455,729,728
  -892,524,684
  -689,845,-530
  423,-701,434
  7,-33,-71
  630,319,-379
  443,580,662
  -789,900,-551
  459,-707,401

  --- scanner 1 ---
  686,422,578
  605,423,415
  515,917,-361
  -336,658,858
  95,138,22
  -476,619,847
  -340,-569,-846
  567,-361,727
  -460,603,-452
  669,-402,600
  729,430,532
  -500,-761,534
  -322,571,750
  -466,-666,-811
  -429,-592,574
  -355,545,-477
  703,-491,-529
  -328,-685,520
  413,935,-424
  -391,539,-444
  586,-435,557
  -364,-763,-893
  807,-499,-711
  755,-354,-619
  553,889,-390

  --- scanner 2 ---
  649,640,665
  682,-795,504
  -784,533,-524
  -644,584,-595
  -588,-843,648
  -30,6,44
  -674,560,763
  500,723,-460
  609,671,-379
  -555,-800,653
  -675,-892,-343
  697,-426,-610
  578,704,681
  493,664,-388
  -671,-858,530
  -667,343,800
  571,-461,-707
  -138,-166,112
  -889,563,-600
  646,-828,498
  640,759,510
  -630,509,768
  -681,-892,-333
  673,-379,-804
  -742,-814,-386
  577,-820,562

  --- scanner 3 ---
  -589,542,597
  605,-692,669
  -500,565,-823
  -660,373,557
  -458,-679,-417
  -488,449,543
  -626,468,-788
  338,-750,-386
  528,-832,-391
  562,-778,733
  -938,-730,414
  543,643,-506
  -524,371,-870
  407,773,750
  -104,29,83
  378,-903,-323
  -778,-728,485
  426,699,580
  -438,-605,-362
  -469,-447,-387
  509,732,623
  647,635,-688
  -868,-804,481
  614,-800,639
  595,780,-596

  --- scanner 4 ---
  727,592,562
  -293,-554,779
  441,611,-461
  -714,465,-776
  -743,427,-804
  -660,-479,-426
  832,-632,460
  927,-485,-438
  408,393,-506
  466,436,-512
  110,16,151
  -258,-428,682
  -393,719,612
  -211,-452,876
  808,-476,-593
  -575,615,604
  -485,667,467
  -680,325,-822
  -627,-443,-432
  872,-547,-609
  833,512,582
  807,604,487
  839,-516,451
  891,-625,532
  -652,-548,-490
  30,-46,-14
`);

type Transform = (c: Coord) => Coord;

const facings: Transform[] = [
  ([x, y, z]) => [+x, +y, +z],
  ([x, y, z]) => [-x, -y, +z],
  ([x, y, z]) => [+y, -x, +z],
  ([x, y, z]) => [-y, +x, +z],
  ([x, y, z]) => [+z, +y, -x],
  ([x, y, z]) => [-z, +y, +x],
];

const upwards: Transform[] = [
  ([x, y, z]) => [+x, +y, +z],
  ([x, y, z]) => [+x, +z, -y],
  ([x, y, z]) => [+x, -y, -z],
  ([x, y, z]) => [+x, -z, +y],
];

function makeSubtractor([x1, y1, z1]: Coord): Transform {
  return ([x2, y2, z2]) => [x2 - x1, y2 - y1, z2 - z1];
}

function makeAdder([x1, y1, z1]: Coord): Transform {
  return ([x2, y2, z2]) => [x2 + x1, y2 + y1, z2 + z1];
}

function compose(...ts: Transform[]): Transform {
  return (c: Coord) => ts.reduceRight((acc, t) => t(acc), c);
}

const setify = (coords: Coord[]): Set<string> =>
  new Set(coords.map((p) => JSON.stringify(p)));

function findAlignment(a: Scanner, b: Scanner) {
  const aStrings = setify(a);
  for (const t2 of facings) {
    for (const t3 of upwards) {
      for (const p1 of a) {
        const t4 = makeAdder(p1);
        for (const p2 of b) {
          const t1 = makeSubtractor(p2);
          const transform = compose(t4, t3, t2, t1);
          const aligned = b.map(transform);
          const matchCount = aligned.filter((p) =>
            aStrings.has(JSON.stringify(p))
          ).length;
          if (matchCount >= 12) {
            return { aligned, transform };
          }
        }
      }
    }
  }
  return undefined;
}

assertEquals(findAlignment(example[0], example[1])?.aligned?.length, 25);
assertEquals(findAlignment(example[1], example[3])?.aligned?.length, 25);
assertEquals(findAlignment(example[1], example[4])?.aligned?.length, 26);

const findAlignments = (
  input: Scanner[],
): { allPointsSize: number; scannerPositons: Coord[] } => {
  const scannerPositons: Coord[] = [];
  const allPoints: Set<string> = setify(input[0]);
  const alignedIndices: number[] = [];

  const alignWith = (index: number, transformSoFar: Transform) => {
    const start = input[index];
    alignedIndices.push(index);
    scannerPositons.push(transformSoFar([0, 0, 0]));

    for (let i = 0; i < input.length; i++) {
      if (alignedIndices.includes(i)) continue;

      const next = input[i];

      const alignment = findAlignment(start, next);
      if (alignment !== undefined) {
        setify(alignment.aligned.map(transformSoFar)).forEach((p) =>
          allPoints.add(p)
        );
        const nextTransform = compose(transformSoFar, alignment.transform);
        alignWith(i, nextTransform);
      }
    }
  };

  const noopTransform = compose();
  alignWith(0, noopTransform);

  if (alignedIndices.length < input.length) {
    throw new Error(
      `Left with ${input.length - alignedIndices.length} unaligned`,
    );
  }

  return { allPointsSize: allPoints.size, scannerPositons };
};

const part1 = (input: Scanner[]): number => {
  return findAlignments(input).allPointsSize;
};

assertEquals(part1(example), 79);

console.log("Result part 1: " + part1(input));

const part2 = (input: Scanner[]): number => {
  const { scannerPositons } = findAlignments(input);

  let maxDistance = -Infinity;
  for (const [x1, y1, z1] of scannerPositons) {
    for (const [x2, y2, z2] of scannerPositons) {
      const distance = Math.abs(x1 - x2) + Math.abs(y1 - y2) +
        Math.abs(z1 - z2);
      if (distance > maxDistance) maxDistance = distance;
    }
  }

  return maxDistance;
};

assertEquals(part2(example), 3621);

console.log("Result part 2: " + part2(input));
