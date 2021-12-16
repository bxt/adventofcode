import { assertEquals, assertMatch } from "./dev_deps.ts";
import { handleFlags } from "./handleFlags.ts";

const defaultFlags = ["--curve", "hilbert"];

const defaultOptions = {
  curve: "hilbert",
  dotFraction: 3,
  drawLine: false,
  height: 500,
  margin: 20,
  recursion: 5,
  result: "success",
  width: 500,
};

Deno.test("handleFlags", () => {
  assertEquals(handleFlags(defaultFlags), defaultOptions);
});

Deno.test("handleFlags > help", () => {
  assertEquals(handleFlags(["--help"]).result, "help");

  const emptyFlags = handleFlags([]);
  assertEquals(emptyFlags.result, "error");
  if (emptyFlags.result !== "error") throw new Error("unreachable");
  assertMatch(
    emptyFlags.error,
    /^curve must be included in ([A-Za-z]+, )*[A-Za-z]+$/,
  );
});

Deno.test("handleFlags > curve", () => {
  assertEquals(handleFlags(["--curve=gosper"]), {
    ...defaultOptions,
    curve: "gosper",
  });
  assertEquals(
    handleFlags(["--curve=invalid"]).result,
    "error",
  );
  assertEquals(handleFlags(["--curve=0"]).result, "error");
});

Deno.test("handleFlags > width", () => {
  assertEquals(handleFlags([...defaultFlags, "--width=1234"]), {
    ...defaultOptions,
    width: 1234,
  });
  assertEquals(handleFlags([...defaultFlags, "--width=string"]), {
    error: "width must be an integer > 0",
    result: "error",
  });
  assertEquals(handleFlags([...defaultFlags, "--width=-2"]), {
    error: "width must be an integer > 0",
    result: "error",
  });
  assertEquals(handleFlags([...defaultFlags, "--width=1.5"]), {
    error: "width must be an integer > 0",
    result: "error",
  });
  assertEquals(handleFlags([...defaultFlags, "--width=0"]), {
    error: "width must be an integer > 0",
    result: "error",
  });
});

Deno.test("handleFlags > height", () => {
  assertEquals(handleFlags([...defaultFlags, "--height=1234"]), {
    ...defaultOptions,
    height: 1234,
  });
  assertEquals(handleFlags([...defaultFlags, "--height=string"]), {
    error: "height must be an integer > 0",
    result: "error",
  });
  assertEquals(handleFlags([...defaultFlags, "--height=-2"]), {
    error: "height must be an integer > 0",
    result: "error",
  });
  assertEquals(handleFlags([...defaultFlags, "--height=1.5"]), {
    error: "height must be an integer > 0",
    result: "error",
  });
  assertEquals(handleFlags([...defaultFlags, "--height=0"]), {
    error: "height must be an integer > 0",
    result: "error",
  });
});

Deno.test("handleFlags > margin", () => {
  assertEquals(handleFlags([...defaultFlags, "--margin=123"]), {
    ...defaultOptions,
    margin: 123,
  });
  assertEquals(handleFlags([...defaultFlags, "--margin=0"]), {
    ...defaultOptions,
    margin: 0,
  });
  assertEquals(handleFlags([...defaultFlags, "--margin=string"]), {
    error: "margin must be an integer >= 0",
    result: "error",
  });
  assertEquals(handleFlags([...defaultFlags, "--margin=-2"]), {
    error: "margin must be an integer >= 0",
    result: "error",
  });
  assertEquals(handleFlags([...defaultFlags, "--margin=1.5"]), {
    error: "margin must be an integer >= 0",
    result: "error",
  });
});

Deno.test("handleFlags > width & margin", () => {
  assertEquals(handleFlags([...defaultFlags, "--width=100", "--margin=50"]), {
    ...defaultOptions,
    width: 100,
    margin: 50,
  });
  assertEquals(handleFlags([...defaultFlags, "--width=100", "--margin=51"]), {
    error: "margin must fit into width and height",
    result: "error",
  });
  assertEquals(handleFlags([...defaultFlags, "--width=101", "--margin=51"]), {
    error: "margin must fit into width and height",
    result: "error",
  });
  assertEquals(handleFlags([...defaultFlags, "--width=102", "--margin=51"]), {
    ...defaultOptions,
    width: 102,
    margin: 51,
  });
});

Deno.test("handleFlags > dotFraction", () => {
  assertEquals(handleFlags([...defaultFlags, "--dotFraction=2.5"]), {
    ...defaultOptions,
    dotFraction: 2.5,
  });
  assertEquals(handleFlags([...defaultFlags, "--dotFraction=0"]), {
    error: "dotFraction must be a number > 0",
    result: "error",
  });
  assertEquals(handleFlags([...defaultFlags, "--dotFraction=string"]), {
    error: "dotFraction must be a number > 0",
    result: "error",
  });
  assertEquals(handleFlags([...defaultFlags, "--dotFraction=-2"]), {
    error: "dotFraction must be a number > 0",
    result: "error",
  });
});

Deno.test("handleFlags > drawLine", () => {
  assertEquals(handleFlags([...defaultFlags, "--drawLine"]), {
    ...defaultOptions,
    drawLine: true,
  });
});

Deno.test("handleFlags > recursion", () => {
  assertEquals(handleFlags([...defaultFlags, "--recursion=1"]), {
    ...defaultOptions,
    recursion: 1,
  });
  assertEquals(handleFlags([...defaultFlags, "--recursion=12"]), {
    ...defaultOptions,
    recursion: 12,
  });
  assertEquals(handleFlags([...defaultFlags, "--recursion=13"]), {
    error: "recursion must be an integer between 1 and 12",
    result: "error",
  });
  assertEquals(handleFlags([...defaultFlags, "--recursion=string"]), {
    error: "recursion must be an integer between 1 and 12",
    result: "error",
  });
  assertEquals(handleFlags([...defaultFlags, "--recursion=-2"]), {
    error: "recursion must be an integer between 1 and 12",
    result: "error",
  });
  assertEquals(handleFlags([...defaultFlags, "--recursion=1.5"]), {
    error: "recursion must be an integer between 1 and 12",
    result: "error",
  });
  assertEquals(handleFlags([...defaultFlags, "--recursion=0"]), {
    error: "recursion must be an integer between 1 and 12",
    result: "error",
  });
});
