package day05

import scala.io.Source
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    assert(stepsToExit(Array(0, 3, 0, 1, -3)) == 5)
    assert(stepsToExit(Array(0, 3, 0, 1, -3), Some(3)) == 10)

    val input = Source.fromResource("day05/input.txt").getLines().map(_.toInt).toList
    println(stepsToExit(input)) // 372671
    println(stepsToExit(input, Some(3))) // 25608480
  }

  def stepsToExit(jumps: Seq[Int], decreaseAfter: Int): Int = stepsToExit(jumps, Some(decreaseAfter))
  def stepsToExit(jumps: Seq[Int]): Int = stepsToExit(jumps, None)

  def stepsToExit(jumps: Seq[Int], decreaseAfter: Option[Int]): Int = {
    val jumpArray = jumps.toArray
    var index = 0
    var steps = 0

    while (jumpArray.isDefinedAt(index)) {
      val jump = jumpArray(index)
      val change = decreaseAfter.flatMap{n => if (jump >= n) Some(-1) else None}.getOrElse(1)
      jumpArray.update(index, jump + change)
      index += jump
      steps += 1
    }

    steps
  }
}
