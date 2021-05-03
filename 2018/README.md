Advent of Code 2018
===================

**Spoiler Warning:** If you want to solve the challenges by yourself, don't read the code.

This directory contains the code I used to solve the challenges from [Advent of Code 2018](http://adventofcode.com/2018).

This year I also used [doctest](http://hackage.haskell.org/package/doctest) in
the Haskell code which is a very quick & easy solution for TDD I think
especially with:

    fswatch -o . | xargs -n1 -I{} doctest main.hs

Contents
--------

The following table lists the available solutions by day and language. I also
added a short summary for each day. I marked the solutions which I consider
especially valuable or pretty with a star (☆).

Day | Haskell   | Description
----|-----------|--------------------------------------------------------------
01  | [x][hs01] | [Being clever with modulus or just summing][aoc01]
02  | [x][hs02] | [Diffing and counting][aoc02]
03  | [x][hs03] | [Fabric claim rectangle overlaps][aoc03]
04  | [x][hs04] | [Sleep pattern aggregations][aoc04]
05  | [x][hs05] | [Desctructive chemistry text processing][aoc05]
06  |           | [Manhattan Voronoi][aoc06]
07  | [>][hs07] | [Steps and requirements][aoc07]
Σ   |       5.5 |

Other solutions
---------------

I enjoy looking at how other coders solved the riddles to learn even more. This
year I follow (A-Z):

* [cschell](https://github.com/cschell/adventofcode/tree/master/2018) (Ruby, Python)
* [fdlk](https://github.com/fdlk/advent-2018/tree/master/src/main/kotlin/nl/kelpin/fleur/advent2018) (kotlin)
* [glguy](https://github.com/glguy/advent2018) (Haskell, C++, Rust)
* [msullivan](https://github.com/msullivan/advent-of-code/tree/master/2018) (Python)

 [aoc01]: http://adventofcode.com/2018/day/1
 [aoc02]: http://adventofcode.com/2018/day/2
 [aoc03]: http://adventofcode.com/2018/day/3
 [aoc04]: http://adventofcode.com/2018/day/4
 [aoc05]: http://adventofcode.com/2018/day/5
 [aoc06]: http://adventofcode.com/2018/day/6
 [aoc07]: http://adventofcode.com/2018/day/7
 [hs01]: day01/main.hs
 [hs02]: day02/main.hs
 [hs03]: day03/main.hs
 [hs04]: day04/main.hs
 [hs05]: day05/main.hs
 [hs07]: day07/main.hs
