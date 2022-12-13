Advent of Code 2022
===================

**Spoiler Warning:** If you want to solve the challenges by yourself, don't read the code.

This directory contains the code I used to solve the challenges from [Advent of Code 2022](http://adventofcode.com/2022).

This year I'm going with Rust. Solutins can be run with e.g. `cargo run --bin adventofcode2022day0`. Some days I used TypeScript with [`deno`](https://deno.land/), solutions can be run with `./main.ts` then.

Learnings
---------

* This year I'm going with Rust after having played with it for maybe a week, ugh. So far I can re-use a lot of knowledge from C and Haskell, however it feels like the code has a lot more of `unwrap()`, `?`, `ok_or()`, `*`, `&`, `iter()`, `collect::<_>()`, `try_from` than I am comfortable with.
* Slowly I get how the mutability and borrowing works. However, when approaching day 7, I wanted a tree stuctucre which is notoriously difficult in Rust, there was even [a meme about it](https://old.reddit.com/r/adventofcode/comments/zezjpl/2022_day_7_trying_to_do_aoc_in_c_having_known_the/). When I realized I would probably need a `Rc` and a `RefCell` nested, I got scared, solved it in TS and got back at it with a hacky non-tree algorithm. Maybe on another day, I'll give those a try.
* Compared to Haskell I rellay like that you can just sprinkle a `println!` and the syntax is much more "C like". However, given the rest of the language is so advanced, it feels strange not having monads or at least some syntactic sugar for applicables. At least we have `?` as a replacement for `do`, I guess.
* Compared to TypeScript, I despise having to think about `u8`/`u16`/`usize`/`i32` etc all the time, when in TS it "just works" and the runtime figures it out. Same for the semi-manual memory management, the GC works just fine. So far no days required real performance though.
* I really like the handling around `mut` in combination with things like `while let` and `match`. This allows me to write more traditional code without functional-foo while still feeling somewhat "safe". In TS I don't use `let` or `class` normally, in Haskell you'd have to bring in Monads.
* That beeing said, I was led to write more OOP-style code as well, and I din't like it at all. Especially with the borrowing you really have to think about which objects (or `impl`s) your "methods" should go to and along with that you get all the other OOP headaches.
* For day 11, I want to compare with [this clean C solution](https://github.com/ednl/aoc2022/blob/main/11.c), let's see...

... let's hope for more learnings to come.

Contents
--------

The following table lists the available solutions by day and language. I also
added a short summary for each day.

Day | Deno        | Rust        | Visual     | Description
----|-------------|-------------|------------|------------------------------------
01  | [✓][deno01] | [✓][rust01] |            | [Elves calorie counting][aoc01]
02  | [✓][deno02] | [✓][rust02] |            | [Rock paper scissors scoring][aoc02]
03  |             | [✓][rust03] |            | [Checking common rucksack items][aoc03]
04  |             | [✓][rust04] |            | [Checking range overlap][aoc04]
05  |             | [✓][rust05] |            | [Moving stacks of crates][aoc05]
06  |             | [✓][rust06] |            | [Finding distinct char sequences][aoc06]
07  | [✓][deno07] | [✓][rust07] |            | [Finding recursive directory sizes][aoc07]
08  |             | [✓][rust08] | [✓][vis08] | [Tree house visibility][aoc08]
09  |             | [✓][rust09] | [✓][vis09] | [Snake rope movement][aoc09]
10  |             | [✓][rust10] |            | [CPU and CRT drawing][aoc10]
11  |             | [✓][rust11] |            | [Monkey queues throwing items][aoc11]
12  |             | [✓][rust12] |            | [Shortest path into a hill][aoc12]
13  |             | [✓][rust13] |            | [Parsing and comparing lists][aoc13]
14  |             |             |            |
15  |             |             |            |
16  |             |             |            |
17  |             |             |            |
18  |             |             |            |
19  |             |             |            |
20  |             |             |            |
21  |             |             |            |
22  |             |             |            |
23  |             |             |            |
24  |             |             |            |
25  |             |             |            |
Σ   |           3 |          13 |          2 |

Other solutions
---------------

I enjoy looking at how other coders solved the riddles to learn even more. This
year I follow (A-Z):

* [aleksandar9](https://github.com/aleksandar9/advent-of-code-2022-kotlin) (Kotlin)
* [cschell](https://github.com/cschell/adventofcode/tree/master/2022) (Python)
* [fdlk](https://github.com/fdlk/advent-2022) (Scala)
* [gabrielpedepera](https://github.com/gabrielpedepera/advent-of-code-2022) (Elixir)
* [glguy](https://github.com/glguy/advent/tree/main/solutions/src/2022) (Haskell)
* [jamincan](https://www.reddit.com/user/jamincan) (Rust)
* [jchevertonwynne](https://github.com/jchevertonwynne/advent-of-code-2022) (Rust)
* [lianmakesthings](https://github.com/lianmakesthings/aoc2022) (Python)
* [lvaroqui](https://github.com/lvaroqui/advent-of-code-2022-rust) (Rust)
* [mikededo](https://github.com/mikededo/advent-of-code/tree/main/22) (Go)
* [sirstrahd](https://github.com/sirstrahd/adventofcode/tree/main/src/Aoc2022) (Java)
* [tbilou](https://github.com/tbilou/advent-of-code-2022) (Kotlin)
* [Xaaris](https://github.com/Xaaris/AdventOfGo/tree/master/2022) (Go)
* [xabgesagtx](https://github.com/xabgesagtx/advent-of-code-2022) (Kotlin)


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
 [rust01]: day01/main.rs
 [deno02]: day02/main.ts
 [rust02]: day02/main.rs
 [rust03]: day03/main.rs
 [rust04]: day04/main.rs
 [rust05]: day05/main.rs
 [rust06]: day06/main.rs
 [deno07]: day07/main.ts
 [rust07]: day07/main.rs
 [rust08]: day08/main.rs
 [rust09]: day09/main.rs
 [vis08]: day08/vis.ts
 [vis09]: day09/vis.ts