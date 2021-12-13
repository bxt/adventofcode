import { Frame } from "./deps.ts";

/**
 * Draws a line.
 *
 * The params `x1`, `y1`, `x2` and `y2`, can be floating point values and might
 * even lie outside the drawing area.
 */
export function drawNeatLine(
  image: Frame,
  x1: number,
  y1: number,
  x2: number,
  y2: number,
  color: number,
) {
  const setPixelSafeAt = (x: number, y: number) => {
    if (x < 1 || y < 1 || x > image.width || y > image.height) return;
    image.setPixelAt(x, y, color);
  };

  const deltaX = Math.abs(x2 - x1);
  const deltaY = Math.abs(y2 - y1);

  const m = deltaY / deltaX;

  const signX = x1 > x2 ? -1 : 1;
  const signY = y1 > y2 ? -1 : 1;

  if (deltaY === 0) {
    for (let i = 0; i <= deltaX; i++) {
      setPixelSafeAt(
        Math.ceil(x1 + i * signX),
        y1,
      );
    }
  } else if (m <= 1) {
    for (let i = 0; i <= deltaX; i++) {
      setPixelSafeAt(
        Math.ceil(x1 + i * signX),
        Math.ceil(y1 + i * m * signY),
      );
    }
  } else {
    for (let i = 0; i <= deltaY; i++) {
      setPixelSafeAt(
        Math.ceil(x1 + i / m * signX),
        Math.ceil(y1 + i * signY),
      );
    }
  }
}
