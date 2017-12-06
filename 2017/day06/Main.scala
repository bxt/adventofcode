package day06

import scala.io.Source
import scala.reflect.ClassTag
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    val example = List(0, 2, 7, 0)
    assert(step(example) == List(2, 4, 1, 2))
    assert(stepsToSameAndLoopSize(example) ==  (5, 4))
    assert(findLooop(List(1,2,3,4,5,6,7,5,8)) == (7, 3))

    val input = Source.fromResource("day06/input.txt").getLines().next.split("\\s+").map(_.toInt).toList
    println(stepsToSameAndLoopSize(input)) // (14029,2765)
  }

  def stepsToSameAndLoopSize(input: Seq[Int]): (Int, Int) = {
    findLooop(Stream.iterate(input)(step))
  }

  @tailrec
  def findLooop[A](input: Seq[A], seenWhen: Map[A, Int] = Map.empty[A, Int]): (Int, Int) = {
    val n = seenWhen.size
    val x +: xs = input
    if (seenWhen.contains(x)) (n, n - seenWhen(x))
    else findLooop(xs, seenWhen + (x -> n))
  }

  def step(memory: Seq[Int]): Seq[Int] = {
      val index = memory.indexOf(memory.max)
      val blocks = memory(index)
      val updates = (index, -blocks) +: (1 to blocks).map{ offset => ((index + offset ) % memory.length, 1)}
      accumulate[Int](_ + _, memory, updates)
  }

  def accumulate[A](combinator: (A, A) => A, input: Seq[A], values: Seq[(Int, A)])(implicit ev: ClassTag[A]): Seq[A] = {
    val output = input.toArray
    values.foreach({case (index, value) => output(index) = combinator(output(index), value) })
    output.toList
  }
}
