import { assertEquals, AssertionError } from "./dev_deps.ts";
import { gosper as gosperSystem, kochSnowflake } from "./lindenmayerSystems.ts";
import { getSymbols, mapSymbols, walkInstructions } from "./getCurvePoints.ts";

function assertAlmostEquals(
  actual: number,
  expected: number,
  epsilon = 0.00001,
  msg?: string,
): void {
  const diff = Math.abs(actual - expected);
  if (diff > epsilon) {
    if (!msg) {
      msg =
        `actual: "${actual}" expected to be within ${epsilon} of "${expected}"`;
    }
    throw new AssertionError(msg);
  }
}

Deno.test("getSymbols", () => {
  assertEquals(getSymbols(gosperSystem, 0), "A");
  assertEquals(getSymbols(gosperSystem, 1), "A-B--B+A++AA+B-");
  assertEquals(
    getSymbols(gosperSystem, 2),
    "A-B--B+A++AA+B--+A-BB--B-A++A+B--+A-BB--B-A++A+B+A-B--B+A++AA+B-++A-B--B+A++AA+B-A-B--B+A++AA+B-++A-BB--B-A++A+B-",
  );

  assertEquals(getSymbols(kochSnowflake, 0), "F--F--F");
  assertEquals(
    getSymbols(kochSnowflake, 1),
    "F+F--F+F--F+F--F+F--F+F--F+F",
  );
});

Deno.test("mapSymbols", () => {
  assertEquals(mapSymbols("ABBA+++AA", Math.PI / 3).length, 9);
});

Deno.test("walkInstructions, without turns", () => {
  assertEquals(walkInstructions(mapSymbols("", 0)), [[0, 0]]);
  assertEquals(walkInstructions(mapSymbols("A", 0)), [[0, 0], [1, 0]]);
  assertEquals(walkInstructions(mapSymbols("AB", 0)), [[0, 0], [1, 0], [2, 0]]);

  assertEquals(walkInstructions([{ type: "forward", length: 4 }]), [[0, 0], [
    4,
    0,
  ]]);
  assertEquals(
    walkInstructions([{ type: "forward", length: 42 }, {
      type: "forward",
      length: 1337,
    }]),
    [[0, 0], [42, 0], [42 + 1337, 0]],
  );
});

Deno.test("walkInstructions, with turns", () => {
  const path = walkInstructions(mapSymbols("ABBA+++AA", Math.PI / 3));
  assertEquals(path.slice(0, 5), [[0, 0], [1, 0], [2, 0], [3, 0], [4, 0]]);
  assertEquals(path[5][0], 3);
  assertAlmostEquals(path[5][1], 0);
  assertEquals(path[6][0], 2);
  assertAlmostEquals(path[6][1], 0);
});
