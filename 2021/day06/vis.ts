#!/usr/bin/env deno run --allow-write --allow-read --allow-net
import {
  COLOR_BLUE_1,
  COLOR_BLUE_3,
  COLOR_BLUE_4,
  COLOR_GREEN_3,
  COLOR_GREEN_4,
  Frame,
  GIF,
  mixColors,
  writeText,
} from "../visualisation_utils/mod.ts";
import { BREED_DAYS, DAYS_PART_ONE, input, MATURE_DAYS } from "./main.ts";

console.log("Calculating...");
console.time("calculation");

const gif = new GIF([]);

const COLOR_BACKGROUND = COLOR_BLUE_1;
const COLOR_FISH = COLOR_BLUE_3;
const COLOR_FISH_BURSTING = COLOR_BLUE_4;
const COLOR_COUNTER = COLOR_BLUE_4;
const COLOR_COUNTER_END = COLOR_GREEN_3;
const COLOR_BORN = COLOR_GREEN_4;

const DAYS = DAYS_PART_ONE;
const FRAMES_PER_DAY = 10;

const SPEED = 1;
const AREA = 7;

const size = 1000;
const width = size;
const height = size;

const drawNeatCircle = (
  image: Frame,
  x: number,
  y: number,
  radius: number,
  color: number,
) => {
  const fromX = Math.max(1, Math.floor(x - radius));
  const toX = Math.min(Math.ceil(x + radius), width);
  const fromY = Math.max(1, Math.floor(y - radius));
  const toY = Math.min(Math.ceil(y + radius), height);
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

type Fish = {
  daysToBreed: number;
  x: number;
  y: number;
  vx: number;
  vy: number;
};

let fish: Fish[] = input.map((n) => {
  const angle = Math.random() * 2 * Math.PI;
  const diffX = Math.sin(angle);
  const diffY = Math.cos(angle);
  return ({
    daysToBreed: n,
    x: Math.random() * width,
    y: Math.random() * height,
    vx: diffX * SPEED,
    vy: diffY * SPEED,
  });
});

for (let day = 0; day <= DAYS; day++) {
  const isLastDay = day === DAYS;

  for (let frameNumber = 0; frameNumber <= FRAMES_PER_DAY; frameNumber++) {
    const isLastFrame = frameNumber === FRAMES_PER_DAY - 1;
    const dayPercentage = frameNumber / FRAMES_PER_DAY;

    const frameDuration = (isLastDay && isLastFrame) ? 3000 : 30;
    const frame = new Frame(width, height, frameDuration);
    frame.drawBox(1, 1, width, height, COLOR_BACKGROUND);

    fish.forEach(({ x, y, daysToBreed }) => {
      const color = daysToBreed === MATURE_DAYS
        ? mixColors(COLOR_BORN, COLOR_FISH, dayPercentage)
        : daysToBreed <= 0
        ? mixColors(COLOR_FISH, COLOR_FISH_BURSTING, dayPercentage)
        : COLOR_FISH;
      const growthFactor = Math.max(BREED_DAYS - daysToBreed, 1) +
        dayPercentage;
      const radius = Math.sqrt(AREA * growthFactor / (BREED_DAYS + 1)) + 1;
      drawNeatCircle(frame, x, y, radius, color);
    });

    const x = `P1 ${fish.length.toString().padStart(7)}`;
    writeText(frame, x, 16, 16, isLastDay ? COLOR_COUNTER_END : COLOR_COUNTER);

    gif.push(frame);

    fish = fish.flatMap(({ x, y, vx, vy, ...lampy }) => {
      const newX = x + vx;
      const newY = y + vy;
      return ({
        ...lampy,
        x: newX < 1 ? 2 - newX : newX > width ? 2 * width - newX : newX,
        y: newY < 1 ? 2 - newY : newY > height ? 2 * height - newY : newY,
        vx: newX < 1 || newX > width ? -vx : vx,
        vy: newY < 1 || newY > width ? -vy : vy,
      });
    });
  }

  fish = fish.flatMap((lampy) => {
    const { x, y, vx, vy } = lampy;
    if (lampy.daysToBreed <= 0) {
      const damping = 0.9;
      const speed = Math.sqrt(vx * vx + vy * vy) * damping;
      const angle = Math.atan2(vy, vx);
      const diffX1 = Math.cos(angle + Math.PI / 5);
      const diffY1 = Math.sin(angle + Math.PI / 5);
      const diffX2 = Math.cos(angle + 9 * Math.PI / 5);
      const diffY2 = Math.sin(angle + 9 * Math.PI / 5);
      return [{
        ...lampy,
        x: x + diffX1 * SPEED,
        y: y + diffY1 * SPEED,
        vx: diffX1 * speed,
        vy: diffY1 * speed,
        daysToBreed: BREED_DAYS - 1,
      }, {
        ...lampy,
        x: x + diffX2 * SPEED,
        y: y + diffY2 * SPEED,
        vx: diffX2 * speed,
        vy: diffY2 * speed,
        daysToBreed: MATURE_DAYS,
      }];
    } else {
      return [{ ...lampy, daysToBreed: lampy.daysToBreed - 1 }];
    }
  });
}
console.timeEnd("calculation");

console.log("Encoding...");

console.time("encoding");
const bytes = await gif.encode();
console.timeEnd("encoding");

console.log("Writing...");

Deno.writeFile("output.gif", bytes);

console.log("Done!");
