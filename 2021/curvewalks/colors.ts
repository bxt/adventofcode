import {
  COLOR_BLUE_1,
  COLOR_BLUE_2,
  COLOR_BLUE_3,
  COLOR_BLUE_4,
  COLOR_GREEN_3,
  COLOR_GREEN_4,
  mixColors,
  range,
} from "./deps.ts";

type ColorScheme = (dotColorCount: number) => {
  dotColors: number[];
  lineColor: number;
  backgroundColor: number;
};

export const adventOfCodeColors: ColorScheme = (dotColorCount: number) => {
  const backgroundColor = COLOR_BLUE_1;
  const lineColor = COLOR_BLUE_2;

  const bluesCount = dotColorCount - 1;
  const dotColors = [
    COLOR_GREEN_4,
    ...range(bluesCount).map((n) =>
      mixColors(COLOR_BLUE_4, backgroundColor, n / bluesCount)
    ),
  ];

  return { dotColors, lineColor, backgroundColor };
};

export const colorfulAoCColors: ColorScheme = (dotColorCount: number) => ({
  dotColors: [
    COLOR_GREEN_4,
    COLOR_GREEN_3,
    COLOR_BLUE_3,
    COLOR_BLUE_4,
    0xff8800ff,
    0xbb7700ff,
    0x990000ff,
    0x660000ff,
  ].slice(0, dotColorCount),
  lineColor: COLOR_BLUE_2,
  backgroundColor: COLOR_BLUE_1,
});
