package day05

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val example = List(0, 3, 0, 1, -3)
    assert(stepsToExit(example)    ==  5)
    assert(stepsToExit(example, 3) == 10)

    val input = Source.fromResource("day05/input.txt").getLines().map(_.toInt).toList
    println(stepsToExit(input)) // 372671
    println(stepsToExit(input, 3)) // 25608480
  }

  def stepsToExit(jumps: Seq[Int], decreaseAfter: Int): Int = stepsToExit(jumps, Some(decreaseAfter))
  def stepsToExit(jumps: Seq[Int]): Int                     = stepsToExit(jumps, None)

  def stepsToExit(jumps: Seq[Int], decreaseAfter: Option[Int]): Int = {
    val jumpArray = jumps.toArray
    var index = 0
    var steps = 0

    while (jumpArray.isDefinedAt(index)) {
      val jump = jumpArray(index)
      val change = decreaseAfter.fold(1){n => if (jump >= n) -1 else 1}
      jumpArray(index) = jump + change
      index += jump
      steps += 1
    }

    steps
  }
}
