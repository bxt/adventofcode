import { mixColors } from "./colors.ts";
import { Frame } from "./deps.ts";

/**
 * Draws an anti aliased circle.
 *
 * The params `x`, `y`, and `radius` can be floating point values and might
 * even lie outside the drawing area. It takes the current colors into account
 * to make a smooth transition between the circle and what is already in the
 * image. It's not super fast, but acceptable.
 */
export const drawNeatCircle = (
  image: Frame,
  x: number,
  y: number,
  radius: number,
  color: number,
) => {
  const fromX = Math.max(1, Math.floor(x - radius));
  const toX = Math.min(Math.ceil(x + radius), image.width);
  const fromY = Math.max(1, Math.floor(y - radius));
  const toY = Math.min(Math.ceil(y + radius), image.height);

  for (let currentY = fromY; currentY <= toY; currentY++) {
    for (let currentX = fromX; currentX <= toX; currentX++) {
      const distance = Math.sqrt((currentX - x) ** 2 + (currentY - y) ** 2);
      const diff = distance - radius + 0.5;

      if (diff < 0) { // inside
        image.setPixelAt(currentX, currentY, color);
      } else if (diff < 1) { // on border
        const mixedColor = mixColors(
          color,
          image.getPixelAt(currentX, currentY),
          diff,
        );
        image.setPixelAt(currentX, currentY, mixedColor);
      }
    }
  }
};
