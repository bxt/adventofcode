Advent of Code 2015
===================

**Spoiler Warning:** If you want to solve the challenges by yourself, don't read the code.

This directory contains the code I used to solve the challenges from [Advent of Code 2015](http://adventofcode.com/2015)

I used Haskell for most of the riddles. Sometimes I conceived a Ruby solution,
if there was a neat application for regexes or OOP. For algorithms which profit
from random access or are just easily faster imperatively, I used C.

Contents
--------

The following table lists the available solutions by day and language. I also
added a short summary for each day. I marked the solutions which I consider
especially valuable or pretty with a star (☆).

Day | C | Haskell | Ruby | Description
----|---|---------|------|-------------
01  | x |   x     |  x   | [Parenthesis counting](http://adventofcode.com/2015/day/1)
02  |   |   x     |      | [Wrapping paper](http://adventofcode.com/2015/day/2)
03  |   |   x     |      | [Routes delivering to houses](http://adventofcode.com/2015/day/3)
04  | ☆ |         |      | [MD5 mining](http://adventofcode.com/2015/day/4)
05  |   |   x     |  x   | [Nice string text processing](http://adventofcode.com/2015/day/5)
06  |   |   x     |  x   | [Light installation](http://adventofcode.com/2015/day/6)
07  |   |   x     |      | [Bitwise signal wiring](http://adventofcode.com/2015/day/7)
08  |   |   x     |  x   | [String escaping](http://adventofcode.com/2015/day/8)
09  |   |   ☆     |      | [TSP](http://adventofcode.com/2015/day/9)
10  |   |   ☆     |      | [Look-and-Say Numbers / RLE](http://adventofcode.com/2015/day/10)
11  | ☆ |   x     |      | [Base26 increment, password rules](http://adventofcode.com/2015/day/11)
12  |   |   x     |      | [JSON number sums](http://adventofcode.com/2015/day/12)
13  |   |   x     |      | [Seating happiness](http://adventofcode.com/2015/day/13)
14  |   |   x     |  ☆   | [Racing Reindeer](http://adventofcode.com/2015/day/14)
15  | x |   x     |      | [Optimize cookie receipt](http://adventofcode.com/2015/day/15)
16  |   |   x     |      | [Aunt Detective](http://adventofcode.com/2015/day/16)
17  | x |   x     |      | [Eggnog container subset sums](http://adventofcode.com/2015/day/17)
18  |   |   x     |      | [GoL with Lights](http://adventofcode.com/2015/day/18)
19  |   |   x     |      | [Medicine with molecules and grammars](http://adventofcode.com/2015/day/19)
20  | x |         |      | [Elves delivering by factor sums](http://adventofcode.com/2015/day/20)
21  |   |   x     |      | [Bossfight with items](http://adventofcode.com/2015/day/21)
22  |   |   x     |      | [Bossfight with Wizard and mana](http://adventofcode.com/2015/day/22)
23  |   |   x     |      | [RAM Computer Simulation](http://adventofcode.com/2015/day/23)
24  |   |   x     |      | [Optimizing partitions](http://adventofcode.com/2015/day/24)
25  |   |   x     |      | [Cantor pairing and residue field](http://adventofcode.com/2015/day/25)
Σ   | 6 |   23    |  5   |

Takeaways
---------

In addition to having a lot of fun solving the puzzles, I also applied some interesting programming techniques for the first time in **C** or **Ruby**:

* Reading a file letter by letter [in C](day01/main.c), [in Haksell](day01/main.hs) and [in Ruby](day01/main.rb).
* Using [Pointer arithmetics, function pointers and calling MD5 in C](day04/main.c).
* Some advanced Regex usage in Ruby [here](day05/main.rb) and [here](day08/main.rb).
* Creating [PNGs with Ruby](day06/main.rb).
* Doing a neat [OOP simulation in Ruby](day14/main.rb).
* Comparing a [naive Haskell solution](day17/main.hs) with a [fast C solution using DP](day17/main.c).

Also I learned and applied these new concepts about **Haskell**:

* Parsec, [very basicly](day02/main.hs) and in [complex situations](day07/main.hs)
* [`->` as reader monad](day09/main.hs)
* [Regexes](day15/main.hs)
* [Arrays](day18/main.hs)
* [Stacking monad transformers](day22/main.hs)
* [Interpreting a very simple programming language](day23/main.hs)

Completion
----------

I finally completed all the puzzles:

![image](https://cloud.githubusercontent.com/assets/639509/12027377/c69893f8-adc5-11e5-9a52-608641c6b3ee.png)

Other solutions
---------------

I enjoy looking at how other coder solved the riddles. This year I followed (A-Z):

* [cschell](https://github.com/cschell/adventofcode/tree/master/2015) (Ruby)
* [darinc](https://github.com/darinc/AdventOfCode) (many languages)
* [glguy](https://github.com/glguy/advent2015) (Haskell)
