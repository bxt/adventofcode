import { Frame } from "./deps.ts";

export const COLOR_BLUE_1 = 0x001122ff;
export const COLOR_BLUE_2 = 0x003366ff;
export const COLOR_BLUE_3 = 0x0055AAff;
export const COLOR_BLUE_4 = 0x0077EEff;
export const COLOR_GREEN_3 = 0x009900ff;
export const COLOR_GREEN_4 = 0x00BB00ff;

export const mixColors = (
  color1: number,
  color2: number,
  color2Percentage: number,
): number => {
  const [r1, g1, b1] = Frame.colorToRGB(color1);
  const [r2, g2, b2] = Frame.colorToRGB(color2);
  const color1Percentage = 1 - color2Percentage;
  const r = r1 * color1Percentage + r2 * color2Percentage;
  const g = g1 * color1Percentage + g2 * color2Percentage;
  const b = b1 * color1Percentage + b2 * color2Percentage;
  return Frame.rgbToColor(r, g, b);
};
