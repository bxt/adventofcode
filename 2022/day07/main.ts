#!/usr/bin/env deno run --allow-read
import { assertEquals } from "https://deno.land/std@0.116.0/testing/asserts.ts";
import { matchGroups, sum } from "../../2020/utils.ts";

const parseInput = (string: string): [string, string[]][] =>
  string.trim().split("$ ").filter((b) => b).map((block) => {
    const [command, ...outputLines] = block.trim().split("\n");
    return [command, outputLines];
  });

const text = await Deno.readTextFile("input.txt");

const input = parseInput(text);

type Directory = {
  name: string;
  size: number;
  totalSize: number;
  children: Directory[];
};

const dirMatcher = matchGroups(/^(?<sizeString>\d+) (?<filename>.+)$/);

const part1 = (input: [string, string[]][]): number => {
  const root = { name: "", size: 0, totalSize: -1, children: [] };
  const currentDirs: Directory[] = [];

  for (const [command, lines] of input) {
    const [bin, ...args] = command.split(" ");
    if (bin === "cd") {
      if (lines.length) {
        throw new Error(`Can not handle lines for cd: ${lines}`);
      }
      const [path] = args;
      if (path === "..") currentDirs.pop();
      else if (path === "/") {
        while (currentDirs.length) currentDirs.pop();
        currentDirs.push(root);
      } else {
        let child = currentDirs.at(-1)?.children.find((c) => c.name === path);
        if (!child) {
          child = { name: path, size: 0, totalSize: -1, children: [] };
          currentDirs.at(-1)?.children.push(child);
        }
        currentDirs.push(child);
      }
    } else if (bin === "ls") {
      if (args.length) {
        throw new Error(`Can not handle args for ls: ${args}`);
      }
      const currentDir = currentDirs.at(-1);
      if (!currentDir) throw new Error("Running ls before a cd");
      currentDir.size = 0;
      for (const line of lines) {
        if (line.startsWith("dir ")) continue;
        const { sizeString } = dirMatcher(line);
        currentDir.size += parseInt(sizeString, 10);
      }
    } else {
      throw new Error(`Unknonw command: ${command}`);
    }
  }

  const figureTotalSize = (dir: Directory): number => {
    return (dir.totalSize = dir.size + sum(dir.children.map(figureTotalSize)));
  };

  figureTotalSize(root);

  const sumSuitableTotalSizes = (dir: Directory): number => {
    const mine = dir.totalSize <= 100000 ? dir.totalSize : 0;
    const others = sum(dir.children.map(sumSuitableTotalSizes));
    return mine + others;
  };

  return sumSuitableTotalSizes(root);
};

const exampleText = await Deno.readTextFile("example.txt");

const example = parseInput(exampleText);

assertEquals(part1(example), 95437);

console.log("Result part 1: " + part1(input));
