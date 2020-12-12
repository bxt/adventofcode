Advent of Code 2020
===================

**Spoiler Warning:** If you want to solve the challenges by yourself, don't read the code.

This directory contains the code I used to solve the challenges from [Advent of Code 2020](http://adventofcode.com/2020).

This year I'm going with TypeScript using [`deno`](https://deno.land/). Solutions can be run with `./main.ts`.

Contents
--------

The following table lists the available solutions by day and language. I also
added a short summary for each day. I marked the solutions which I consider
especially valuable or pretty with a star (☆).

Day | Deno         | Description
----|-------------|--------------------------------------------------------------
01  | [✓][deno01] | [Combining, finding, summing, multiplying][aoc01]
02  | [✓][deno02] | [Password policies][aoc02]
03  | [✓][deno03] | [Walking through a map of tees][aoc03]
04  | [✓][deno04] | [Passport checks][aoc04]
05  | [✓][deno05] | [Airplane boarding with binary numbers][aoc05]
06  | [✓][deno06] | [Person customs declaration forms answer set operations][aoc06]
07  | [✓][deno07] | [Recursive colored bags][aoc07]
08  | [✓][deno08] | [Infinite loops in game boy instructions][aoc08]
09  | [✓][deno09] | [Sum of numbers encryptions][aoc09]
09  | [✓][deno10] | [Jolt adapter combinations][aoc10]
09  | [✓][deno11] | [Game of life with seats][aoc11]
09  | [✓][deno12] | [Ship navigation instructions][aoc12]
Σ   |          12 |

Takeaways
---------

* I really like working with deno, and especially setting it up is a nice experience: you install a single binary for deno and maybe the VS code extension, and already you get package management, type safety, linting, formatting, autocompletion, and a basic testing framework with coverage reports. Setting up the same can get really annoying for node. For languages like Ruby it's almost impossible to do.
* Even though deno sometimes feels a bit "beta", I didn't encounter any severe problems
* TypeScript is very clever and can figure out surprisingly many things at compile-time already.
* However, I feel TypeScript is a bit verbose sometimes, and certainly invites to over-egineer things to sprinkle everything with types. E.g. on day 12 [I came up with a solution][deno12] which works eerie similarly to [this haskell one](https://github.com/glguy/advent2020/blob/master/execs/Day12.hs), but looks a lot more Java-ish...
* Since TS can figure out so many things it's frustrating when it can not, and you run into issues like inferring just one type parameter is [not yet](https://github.com/microsoft/TypeScript/pull/26349) possible, or [`Array.includes` can not "filter" types](https://github.com/microsoft/TypeScript/issues/26255).

Other solutions
---------------

I enjoy looking at how other coders solved the riddles to learn even more. This
year I follow (A-Z):

* [carlastabile](https://github.com/carlastabile/advent-of-code-2020) (Ruby)
* [cschell](https://github.com/cschell/adventofcode/tree/master/2020) (Ruby)
* [etrepum](https://github.com/etrepum/aoc-2020) (PureScript & Dhall)
* [fdlk](https://github.com/fdlk/advent-2020) (R)
* [glguy](https://github.com/glguy/advent2020) (Haskell)
* [senegalo](https://github.com/senegalo/advent-of-code-2020) (Ruby)
* [sophiebits](https://github.com/sophiebits/adventofcode/tree/main/2020) (Python)

 [aoc01]: http://adventofcode.com/2020/day/1
 [aoc02]: http://adventofcode.com/2020/day/2
 [aoc03]: http://adventofcode.com/2020/day/3
 [aoc04]: http://adventofcode.com/2020/day/4
 [aoc05]: http://adventofcode.com/2020/day/5
 [aoc06]: http://adventofcode.com/2020/day/6
 [aoc07]: http://adventofcode.com/2020/day/7
 [aoc08]: http://adventofcode.com/2020/day/8
 [aoc09]: http://adventofcode.com/2020/day/9
 [aoc10]: http://adventofcode.com/2020/day/10
 [aoc11]: http://adventofcode.com/2020/day/11
 [aoc12]: http://adventofcode.com/2020/day/12
 [deno01]: day01/main.ts
 [deno02]: day02/main.ts
 [deno03]: day03/main.ts
 [deno04]: day04/main.ts
 [deno05]: day05/main.ts
 [deno06]: day06/main.ts
 [deno07]: day07/main.ts
 [deno08]: day08/main.ts
 [deno09]: day09/main.ts
 [deno10]: day10/main.ts
 [deno11]: day11/main.ts
 [deno12]: day12/main.ts
