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
10  | [✓][deno10] | [Jolt adapter combinations][aoc10]
11  | [✓][deno11] | [Game of life with seats][aoc11]
12  | [✓][deno12] | [Ship navigation instructions][aoc12]
13  | [✓][deno13] | [Bus arrivals vs. Chinese remainder theorem][aoc13]
14  | [✓][deno14] | [Ship docking computer with bit masks][aoc14]
15  | [✓][deno15] | [Evles playing Van Eck’s memory game][aoc15]
16  | [✓][deno16] | [Ticket field ranges 2SAT][aoc16]
17  | [✓][deno17] | [Conway Cubes engergy source][aoc17]
18  | [✓][deno18] | [Child with operator precedence problems, shunting-Yard to the rescue][aoc18]
19  | [✓][deno19] | [Evles need some parsing help, CYK to the rescue][aoc19]
20  | [✓][deno20] | [Connecting tiles to find sea monsters][aoc20]
21  | [✓][deno21] | [Ingredients and allergens, basically day 16 lol][aoc21]
22  | [✓][deno22] | [Playing Top Trumps with a crab][aoc22]
23  | [✓][deno23] | [Playing cups and balls with a crab][aoc23]
24  | [✓][deno24] | [Hexagonal game of life hotel lobby tiles][aoc24]
25  | [✓][deno25] | [tbd][aoc25]
Σ   |          25 |

Takeaways
---------

* Deno seems great:
  * I really like working with deno, and especially setting it up is a nice experience: you install a single binary for deno (and maybe the VS code extension) and already you get a fully working environment, and also a well equipped-one. Setting up similar tooling can get really annoying for node. For languages like Ruby it's almost impossible to get such a setup. We get:
    * package management
    * type checks
    * linting
    * formatting
    * autocompletion
    * automatic imports
    * a basic testing framework
    * with coverage reports
    * [debugger support](https://deno.land/manual/getting_started/debugging_your_code)
    * documentation, e.g. [here's the docs of the utils file](https://doc.deno.land/https/raw.githubusercontent.com/bxt/adventofcode/main/2020/utils.ts) I used for this year.
    * Since [1.6 you can even export a standalone binary](https://deno.land/posts/v1.6#codedeno-compilecode-self-contained-standalone-binaries).
  * Even though deno sometimes feels a bit "beta", I didn't encounter any severe problems. One thing I found a bit annoying is that the formatter sometimes removed big chunks of code, but I could always get them back with <kbd>⌘</kbd> <kbd>Z</kbd>.
  * It feels great to be able to use all the latest language features and TypeScript consistently across the codebase (including e.g. tests) without having to set up many things. In fact, I have 0 config files, and the defaults always felt reasonable.
  * I did not target browsers though... with those I think things would become more messy.
* Types... there are pros and cons. Adding all the types feels a bit tedious form at times, but it makes up by catching so many small errors all the time:
  * I feel TypeScript is a bit verbose sometimes, and certainly invites to over-engineer things to sprinkle everything with types. E.g. on day 12 [I came up with a solution][deno12] which works eerie similarly to [this haskell one](https://github.com/glguy/advent2020/blob/master/execs/Day12.hs), but looks a lot more Java-ish...
  * Adding all the types means writing more code and I guess does not make it easier to get to the leaderboard in the end.
  * TypeScript is very clever and can figure out surprisingly many things at compile-time already. E.g. [in this switch statement](day14/main.ts#L152-L164) it knows that the variable is changed from `null` in all branches and that always one of the `case`es is called, and would complain otherwise.
  * Since TS can figure out so many things it's frustrating when it can not, and you run into issues like inferring just one type parameter is [not yet](https://github.com/microsoft/TypeScript/pull/26349) possible, or [`Array.includes` can not "filter" types](https://github.com/microsoft/TypeScript/issues/26255).
* JavaScript (and thus also TypeScript) had a bad reupatation for missing a good standard library. While it is not huge still, I found nowadays it comes reasonably equipped, especially for the kind of programming you tend to do for AoC. While things like classes, getters/setters, async, promises, generators are not so interesting, I used some new features extensively:
  * From ES2015: string template literals, arrow functions, object destructuring and spreads, `Set`s, `String.startsWith`, `class`es (only in [day 11][deno11] though)
  * From ES2016 `Array.includes`
  * From ES2017 `Object.values`, and hey: `String​.pad​Start` replaces the infamous `left-pad`
  * From ES2018 spreads and named capture groups
  * From ES2019 `Array.flat` and `Array.flatMap`
* I did end up writing a small library of things I found missing and would expect in a "JS standard libarary", like basic `sum`, `product` helpers, and some set operations. Implementing those was acutally fun, because I could use some OOP, abstract base class, inheritance, getter methods, generators, iterators etc. which I normally try to avoid, as in high level React code they are usually unjustified complexity.
* Seems like days 22, 18, 15, 12, 5 might be suitable for bf/piet

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
 [aoc13]: http://adventofcode.com/2020/day/13
 [aoc14]: http://adventofcode.com/2020/day/14
 [aoc15]: http://adventofcode.com/2020/day/15
 [aoc16]: http://adventofcode.com/2020/day/16
 [aoc17]: http://adventofcode.com/2020/day/17
 [aoc18]: http://adventofcode.com/2020/day/18
 [aoc19]: http://adventofcode.com/2020/day/19
 [aoc20]: http://adventofcode.com/2020/day/20
 [aoc21]: http://adventofcode.com/2020/day/21
 [aoc22]: http://adventofcode.com/2020/day/22
 [aoc23]: http://adventofcode.com/2020/day/23
 [aoc24]: http://adventofcode.com/2020/day/24
 [aoc25]: http://adventofcode.com/2020/day/25
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
 [deno13]: day13/main.ts
 [deno14]: day14/main.ts
 [deno15]: day15/main.ts
 [deno16]: day16/main.ts
 [deno17]: day17/main.ts
 [deno18]: day18/main.ts
 [deno19]: day18/main.ts
 [deno20]: day20/main.ts
 [deno21]: day21/main.ts
 [deno22]: day22/main.ts
 [deno23]: day23/main.ts
 [deno24]: day24/main.ts
 [deno25]: day25/main.ts
