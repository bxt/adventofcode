package day06

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val example = List(0, 2, 7, 0)
    assert(stepsToSameAndLoopSize(example)    ==  (5, 4))

    val input = Source.fromResource("day06/input.txt").getLines().next.split("\\s+").map(_.toInt).toList
    println(stepsToSameAndLoopSize(input)) // (14029,2765)
  }

  def stepsToSameAndLoopSize(input: Seq[Int]): (Int, Int) = {
    var steps = 0
    val seenWhen = scala.collection.mutable.Map[List[Int], Int]()

    val memory = input.toArray
    while (!seenWhen.contains(memory.toList)) {
      seenWhen(memory.toList) = steps

      var index = memory.indexOf(memory.max)
      var blocksLeft = memory(index)
      memory(index) = 0
      index += 1;

      while(blocksLeft > 0) {
        memory(index % memory.length) += 1
        blocksLeft -= 1
        index += 1
      }
      steps += 1
    };

    (steps, steps - seenWhen(memory.toList))
  }
}
