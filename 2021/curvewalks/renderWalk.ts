#!/usr/bin/env deno run --allow-write --allow-read --allow-net
import {
  Coord,
  drawNeatCircle,
  drawNeatLine,
  Frame,
  GIF,
  range,
  slidingWindows,
} from "./deps.ts";

export const renderWalk = ({
  backgroundColor,
  dotColors,
  dotSize,
  frames,
  height,
  lineColor,
  points,
  width,
}: {
  backgroundColor: number;
  dotColors: number[];
  dotSize: number;
  frames: number;
  height: number;
  lineColor: number | undefined;
  points: Coord[];
  width: number;
}): GIF => {
  const gif = new GIF([]);

  for (let colorIndex = 0; colorIndex < dotColors.length; colorIndex++) {
    for (const frameNumber of range(frames)) {
      const framePercentage = frameNumber / frames;
      const frame = new Frame(width, height, 100);
      frame.drawBox(1, 1, width, height, backgroundColor);

      const lines = slidingWindows(points, 2);

      if (lineColor !== undefined) {
        lines.forEach(([[x1, y1], [x2, y2]]) =>
          drawNeatLine(frame, x1, y1, x2, y2, lineColor)
        );
      }

      for (let lineNumber = 0; lineNumber < lines.length; lineNumber++) {
        const half = Math.round(framePercentage);
        if ((half + lineNumber) % 2 == 0) continue;
        const linePercentage = framePercentage * 2 - half;

        const [[x1, y1], [x2, y2]] = lines[lineNumber];
        const x = linePercentage * x1 + (1 - linePercentage) * x2;
        const y = linePercentage * y1 + (1 - linePercentage) * y2;

        const color = dotColors[
          (colorIndex + Math.floor(lineNumber / 2)) % dotColors.length
        ];

        drawNeatCircle(frame, x, y, dotSize, color);
      }

      gif.push(frame);
    }
  }

  return gif;
};
