package day17

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    assert(part1(3, 2017) == 638)
    println(part1(371, 2017)) // 1311

    assert(1.to(9).map(n => part2(3, n)).toList == List(1, 2, 2, 2, 5, 5, 5, 5, 9))
    println(part2(371, 50000000))
  }

  def part1(advance: Int, max: Int): Int = {
    1.to(max).foldLeft(List(0))((list, number) => {
      val (a, b) = list.splitAt(advance % list.length)
      b ++ a ++ List(number)
    }).head
  }

  def part2(advance: Int, max: Int): Int = {
    1.to(max).foldLeft((0, -1))((t, number) => {
      val (index, target) = t
      val newIndex = (index + advance + 1) % number
      val newTarget = if (newIndex == 0) number else target
      (newIndex, newTarget)
    })._2
  }
}
