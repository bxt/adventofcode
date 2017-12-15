Advent of Code 2017
===================

**Spoiler Warning:** If you want to solve the challenges by yourself, don't read the code.

This directory contains the code I used to solve the challenges from [Advent of Code 2017](http://adventofcode.com/2017).

Contents
--------

The following table lists the available solutions by day and language. I also
added a short summary for each day. I marked the solutions which I consider
especially valuable or pretty with a star (☆).

Day | Scala     | Rust      | Description                             
----|-----------|-----------|----------------------------------------------------
01  | [x][sc01] |           | [Sum equal digits][aoc01]
02  | [x][sc02] |           | [Sum spreadsheet min/max][aoc02]
03  | [x][sc03] |           | [Ulam spiral fibonacci mashup][aoc03]
04  | [x][sc04] |           | [Anagram passphrases][aoc04]
05  | [x][sc05] |           | [Weird jump instructions][aoc05]
06  | [x][sc06] |           | [Memory loop debugger][aoc06]
07  | [x][sc07] |           | [Programs disc balance tree][aoc07]
08  |           |           | [I Heard You Like Registers ][aoc08]
09  | [x][sc09] |           | [Garbage and groups stream counting][aoc09]
10  | [x][sc10] |           | [Knot hash][aoc10]
11  | [x][sc11] |           | [Hex grid distances][aoc11]
12  | [x][sc12] |           | [Connected components][aoc12]
13  | [x][sc13] |           | [Modulus stuff like disks last year][aoc13]
13  | [x][sc14] |           | [Grid of knot hash and connected components][aoc14]
13  | [x][sc15] |           | [Generating number list with bitmasks][aoc15]
Σ   |        14 |         0 |

Takeaways
---------

* Scala generally feels like a more sane version of Java, especially if you do
  functional things.
* Scala has really confusing syntax, e.g. `.filter(_.length == 1)` works, but
  `.filter(foo(_).length == 1)` doesn't (the underscore can be many things)
* Scala generally feels like a more ugly version of Haskell, especially if you do
  functional things.
* For example `Some(3).getOrElse(recursion(...))` will not work, because it ain't lazy

Other solutions
---------------

I enjoy looking at how other coders solved the riddles to learn even more. This
year I follow (A-Z):

* [cschell](https://github.com/cschell/adventofcode/tree/master/2017) (Python, Ruby, Rust)
* [fdlk](https://github.com/fdlk/advent-2017/tree/master/src) (Scala)
* [glguy](https://github.com/glguy/advent2017) (Haskell)
* [msullivan](https://github.com/msullivan/advent-of-code/tree/master/2017) (Haskell, Python)

These coders form last year didn't yet push anything this year, but I'll keep an eye open (A-Z):

* [bildzeitung](https://github.com/bildzeitung/) (Python?)
* [Pyrobolser](https://github.com/Pyrobolser/) (C#)

 [aoc01]: http://adventofcode.com/2017/day/1
 [aoc02]: http://adventofcode.com/2017/day/2
 [aoc03]: http://adventofcode.com/2017/day/3
 [aoc04]: http://adventofcode.com/2017/day/4
 [aoc05]: http://adventofcode.com/2017/day/5
 [aoc06]: http://adventofcode.com/2017/day/6
 [aoc07]: http://adventofcode.com/2017/day/7
 [aoc08]: http://adventofcode.com/2017/day/8
 [aoc09]: http://adventofcode.com/2017/day/9
 [aoc10]: http://adventofcode.com/2017/day/10
 [aoc11]: http://adventofcode.com/2017/day/11
 [aoc12]: http://adventofcode.com/2017/day/12
 [aoc13]: http://adventofcode.com/2017/day/13
 [aoc14]: http://adventofcode.com/2017/day/14
 [aoc15]: http://adventofcode.com/2017/day/15
 [sc01]: day01/Main.scala
 [sc02]: day02/Main.scala
 [sc03]: day03/Main.scala
 [sc04]: day04/Main.scala
 [sc05]: day05/Main.scala
 [sc06]: day06/Main.scala
 [sc07]: day07/Main.scala
 [sc09]: day09/Main.scala
 [sc10]: day10/Main.scala
 [sc11]: day11/Main.scala
 [sc12]: day12/Main.scala
 [sc13]: day13/Main.scala
 [sc14]: day14/Main.scala
 [sc15]: day15/Main.scala
