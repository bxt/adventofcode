import { Coord, minMax } from "../../2020/utils.ts";

export const mapToBox = (
  points: Coord[],
  margin: number,
  width: number,
  height: number,
): Coord[] => {
  const usableWidth = (width - 2 * margin);
  const usableHeight = (height - 2 * margin);
  const [minX, maxX] = minMax(points.map(([x, _]) => x));
  const [minY, maxY] = minMax(points.map(([_, y]) => y));
  const diffX = maxX - minX;
  const diffY = maxY - minY;
  const factor = Math.min(
    usableWidth / diffX,
    usableHeight / diffY,
  );
  if (factor === undefined) throw new Error();
  return points.map(([x, y]) => [
    (x - minX - diffX / 2) * factor + margin + usableWidth / 2,
    (y - minY - diffY / 2) * factor + margin + usableHeight / 2,
  ]);
};
