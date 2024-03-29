Advent of Code 2016
===================

**Spoiler Warning:** If you want to solve the challenges by yourself, don't read the code.

This directory contains the code I used to solve the challenges from [Advent of Code 2016](http://adventofcode.com/2016).

Contents
--------

The following table lists the available solutions by day and language. I also
added a short summary for each day. I marked the solutions which I consider
especially valuable or pretty with a star (☆).

Day | Ruby      | Haskell   | Description                             | Letter Language*
----|-----------|-----------|-----------------------------------------|-----------------
01  | [x][rb01] | [x][hs01] | [Manhattan turtle walking][aoc01]       | [AWK](day01/main.awk)
02  | [x][rb02] |           | [Bathroom code pad walking][aoc02]      | [Brainfuck](day02/part1.bf) and [Ook!](day02/part1.ook)
03  | [x][rb03] |           | [(Im)possible triangles][aoc03]         | CoffeeScript
04  | [x][rb04] |           | [Room checksums and encryption][aoc04]  | C++
05  |           |           | [MD5 door code search][aoc05]           | [C](day05/main.c)
06  | [x][rb06] | [x][hs06] | [Jammed Santa repetition code][aoc06]   | D
07  | [x][rb07] |           | [Swedish palindrome IPV7][aoc07]        | Elixir
08  | [x][rb08] |           | [2FA display commands][aoc08]           | Erlang
09  | [x][rb09] |           | [Decompress by multiply][aoc09]         | Flow.js
10  | [x][rb10] |           | [Balance Bot Network][aoc10]            | Go
11  |           |           | [Radiation in elevators][aoc11]         | Hack
12  | [x][rb12] | [x][hs12] | [Leonardo's Monorail machine][aoc12]    | [Java](day12/Main.java)
13  | [x][rb13] |           | [Cubicles Maze Shortest Path][aoc13]    | Kotlin
14  | [x][rb14] |           | [MD5 Key search][aoc14]                 | Lisp
15  | [x][rb15] |           | [Kinetic disc modulus sculpture][aoc15] | [Matlab](day15/main.m)
16  | [x][rb16] | [x][hs16] | [Generate Binary disk checksum][aoc16]  | Objective-C
17  | [x][rb17] |           | [MD5 doors in 4x4 rooms vault][aoc17]   | Python
18  | [x][rb18] |           | [Cellular tiled floor][aoc18]           | Processing
19  | [x][rb19] |           | [Elves stealing White Elephants][aoc19] | PHP
20  | [x][rb20] |           | [IP range block list][aoc20]            | [Rust](day20/main.rs)
21  | [x][rb21] |           | [Password scrambling][aoc21]            | [Scala](day21/Main.scala)
22  | [x][rb22] |           | [15-puzzle grid computing moves][aoc22] | TypeScipt
23  | [x][rb23] |           | [Safe cracking machine][aoc23]          | Visual Basic
24  | [x][rb24] |           | [Duct round trips][aoc24]               | Wolfram Language
25  | [x][rb25] |           | [Clock signal in assembunny][aoc25]     | Zsh
Σ   |        23 |         4 |                                         | * coming soon

Takeaways
---------

In addition to having a lot of fun solving the puzzles, I also learned some interesting programming techniques:

* [Writing programs in a language named Brainfuck](day02/part1.bf)
* Creating a [debugger/IDE thingy using flow and React for Brainfuck](https://gitlab.com/bxt/brainfuck-debugger) with CI testing
* Pointer and value semantics [in C structs](day05/main.c)
* Making a [script runnable via `rspec` or `ruby`][rb07]
* [Global methods in Ruby](https://gist.github.com/bxt/b4da635da9aee12d6e7236147513e40f)
* Speed and languages: [Day 12](day12/) has fast (3s) Haskell solution, which is slow through `runhaskell` (350s), a long and fast (<1s) Java solution and a Ruby solution in between (15s). However, [building a C program with AWK](https://www.reddit.com/r/adventofcode/comments/5hus40/2016_day_12_solutions/db36od6/) and compiling builds the solution instantly.
* Optimizing a Ruby script: [MD5ing in day 14][rb14] runs 80s, with fork parallelism 40s, with critical paths in C 13s, with both 5s.
* Optimizing a Crystal program: [MD5ing in day 14](day14/main.cr) runs 40s, with OpenSSL's md5 20s (using ugly Pointers does not help), with fork parallelism 8s, but compiling takes an additional 11s.
* Optimizing a Scala program: [MD5ing in day 14](day14/Main.scala) ran 740s with a md5 found on StackOverflow, with a better formatter 166s and 46s with a specialized formatter.
* Ruby 2.4.0 was released at the end of AoC: The new hash improvements were not noticable in Day 14's script. But the new `Array#sum` method (see 0424ecbb2d37b68f007bb1e44005de79ed7c9ef3) and the new `Regex#match?` were already useful.

Leaderboard
-----------

This year I sometimes tried to get on the daily leaderboard. And indeed I managed to get there on 4 days 1, 6, 8 and 11 scoring a total of 184 points. If the overall leaderboard was longer I'd have position 272 of about 1300 people who completed the puzzle or 13k who started it.

Other solutions
---------------

I enjoy looking at how other coder solved the riddles. This year I followed (A-Z):

* [bildzeitung](https://github.com/bildzeitung/2016adventofcode) (Python)
* [cschell](https://github.com/cschell/adventofcode/tree/master/2016) (Elixir)
* [fdlk](https://github.com/fdlk/advent-2016/tree/master/src) (Scala)
* [glguy](https://github.com/glguy/advent2016) (Haskell)
* [msullivan](https://github.com/msullivan/advent-of-code/tree/master/2016) (Haskell, Python)
* [Pyrobolser](https://github.com/Pyrobolser/AdventOfCode2016/tree/master/AdventOfCode2016/Days) (C#)

 [aoc01]: http://adventofcode.com/2016/day/1
 [aoc02]: http://adventofcode.com/2016/day/2
 [aoc03]: http://adventofcode.com/2016/day/3
 [aoc04]: http://adventofcode.com/2016/day/4
 [aoc05]: http://adventofcode.com/2016/day/5
 [aoc06]: http://adventofcode.com/2016/day/6
 [aoc07]: http://adventofcode.com/2016/day/7
 [aoc08]: http://adventofcode.com/2016/day/8
 [aoc09]: http://adventofcode.com/2016/day/9
 [aoc10]: http://adventofcode.com/2016/day/10
 [aoc11]: http://adventofcode.com/2016/day/11
 [aoc12]: http://adventofcode.com/2016/day/12
 [aoc13]: http://adventofcode.com/2016/day/13
 [aoc14]: http://adventofcode.com/2016/day/14
 [aoc15]: http://adventofcode.com/2016/day/15
 [aoc16]: http://adventofcode.com/2016/day/16
 [aoc17]: http://adventofcode.com/2016/day/17
 [aoc18]: http://adventofcode.com/2016/day/18
 [aoc19]: http://adventofcode.com/2016/day/19
 [aoc20]: http://adventofcode.com/2016/day/20
 [aoc21]: http://adventofcode.com/2016/day/21
 [aoc22]: http://adventofcode.com/2016/day/22
 [aoc23]: http://adventofcode.com/2016/day/23
 [aoc24]: http://adventofcode.com/2016/day/24
 [aoc25]: http://adventofcode.com/2016/day/25
 [rb01]: day01/main.rb
 [rb02]: day02/main.rb
 [rb03]: day03/main.rb
 [rb04]: day04/main.rb
 [rb06]: day06/main.rb
 [rb07]: day07/main.rb
 [rb08]: day08/main.rb
 [rb09]: day09/main.rb
 [rb10]: day10/main.rb
 [rb12]: day12/main.rb
 [rb13]: day13/main.rb
 [rb14]: day14/main.rb
 [rb15]: day15/main.rb
 [rb16]: day16/main.rb
 [rb17]: day17/main.rb
 [rb18]: day18/main.rb
 [rb19]: day19/main.rb
 [rb20]: day20/main.rb
 [rb21]: day21/main.rb
 [rb22]: day22/main.rb
 [rb23]: day23/main.rb
 [rb24]: day24/main.rb
 [rb25]: day25/main.rb
 [hs01]: day01/main.hs
 [hs06]: day06/main.hs
 [hs12]: day12/main.hs
 [hs16]: day16/main.hs
