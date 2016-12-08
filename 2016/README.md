Advent of Code 2016
===================

**Spoiler Warning:** If you want to solve the challenges by yourself, don't read the code.

This directory contains the code I used to solve the challenges from [Advent of Code 2016](http://adventofcode.com/2016)

Contents
--------

The following table lists the available solutions by day and language. I also
added a short summary for each day. I marked the solutions which I consider
especially valuable or pretty with a star (☆).

Day | Ruby | Haskell | Description                                                         | Letter Language*
----|------|---------|---------------------------------------------------------------------|-----------------
01  |  x   |    x    | [Manhattan turtle walking](http://adventofcode.com/2016/day/1)      | AWK
02  |  x   |         | [Bathroom code pad walking](http://adventofcode.com/2016/day/2)     | [Brainfuck](day02/part1.bf)
03  |  x   |         | [(Im)possible triangles](http://adventofcode.com/2016/day/3)        | CoffeeScript
04  |  x   |         | [Room checksums and encryption](http://adventofcode.com/2016/day/4) | C++
05  |      |         | [MD5 door code search](http://adventofcode.com/2016/day/5)          | [C](day05/main.c)
06  |  x   |    x    | [Jammed Santa repetition code](http://adventofcode.com/2016/day/6)  | D
07  |  x   |         | [Swedish palindrome IPV7](http://adventofcode.com/2016/day/7)       | Elixir
08  |  x   |         | [2FA display commands](http://adventofcode.com/2016/day/8)          | Erlang
09  |      |         | [tba](http://adventofcode.com/2016/day/9)                           | Flow.js
10  |      |         | [tba](http://adventofcode.com/2016/day/10)                          | Go
11  |      |         | [tba](http://adventofcode.com/2016/day/11)                          | Hack
12  |      |         | [tba](http://adventofcode.com/2016/day/12)                          | Java
13  |      |         | [tba](http://adventofcode.com/2016/day/13)                          | Kotlin
14  |      |         | [tba](http://adventofcode.com/2016/day/14)                          | Lisp
15  |      |         | [tba](http://adventofcode.com/2016/day/15)                          | Matlab
16  |      |         | [tba](http://adventofcode.com/2016/day/16)                          | Objective-C
17  |      |         | [tba](http://adventofcode.com/2016/day/17)                          | PHP
18  |      |         | [tba](http://adventofcode.com/2016/day/18)                          | Python
19  |      |         | [tba](http://adventofcode.com/2016/day/19)                          | Processing
20  |      |         | [tba](http://adventofcode.com/2016/day/20)                          | Rust
21  |      |         | [tba](http://adventofcode.com/2016/day/21)                          | Swift
22  |      |         | [tba](http://adventofcode.com/2016/day/22)                          | TypeScipt
23  |      |         | [tba](http://adventofcode.com/2016/day/23)                          | Visual Basic
24  |      |         | [tba](http://adventofcode.com/2016/day/24)                          | Wolfram Language
25  |      |         | [tba](http://adventofcode.com/2016/day/25)                          | Zsh
Σ   |  6   |    1    |                                                                     | * coming soon

Takeaways
---------

In addition to having a lot of fun solving the puzzles, I also learned some interesting programming techniques:

* [Writing programs in a language named Brainfuck](day02/part1.bf)
* Creating a [debugger/IDE thingy using flow and React for Brainfuck](https://gitlab.com/bxt/brainfuck-debugger) with CI testing
* Pointer and value semantics [in C structs](day05/main.c)
* Making a [script runnable via `rspec` or `ruby`](day07/main.rb)
* [Global methods in Ruby](https://gist.github.com/bxt/b4da635da9aee12d6e7236147513e40f)

Other solutions
---------------

I enjoy looking at how other coder solved the riddles. This year I followed (A-Z):

* [bildzeitung](https://github.com/bildzeitung/2016adventofcode) (Python)
* [cschell](https://github.com/cschell/adventofcode/tree/master/2016) (Elixir)
* [glguy](https://github.com/glguy/advent2016) (Haskell)
* [msullivan](https://github.com/msullivan/advent-of-code/tree/master/2016) (Haskell, Python)
* [Pyrobolser](https://github.com/Pyrobolser/AdventOfCode2016/tree/master/AdventOfCode2016/Days) (C#)
