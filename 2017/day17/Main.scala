package day17

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    assert(part1(3, 2017) == 638)
    println(part1(371, 2017)) // 1311
  }

  def part1(advance: Int, max: Int): Int = {
    1.to(max).foldLeft(List(0))((list, number) => {
      val (a, b) = list.splitAt(advance % list.length)
      b ++ a ++ List(number)
    }).head
  }
}
