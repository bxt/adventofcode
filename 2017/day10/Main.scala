package day10

import scala.io.Source

object Main {
  val LIST_LENGTH = 256

  def main(args: Array[String]): Unit = {
    assert(wrapingReverse(List(0, 1, 2, 3, 4), 0, 3) == List(2, 1, 0, 3, 4))
    assert(wrapingReverse(List(2, 1, 0, 3, 4), 3, 4) == List(4, 3, 0, 1, 2))
    assert(wrapingReverse(List(4, 3, 0, 1, 2), 3, 1) == List(4, 3, 0, 1, 2))
    assert(wrapingReverse(List(4, 3, 0, 1, 2), 1, 5) == List(3, 4, 2, 1, 0))

    val input = Source.fromResource("day10/input.txt").getLines().next
    val lengths = input.split(",").map(_.toInt).toList
    val starts = lengths.zipWithIndex.map(x => x._1 + x._2).scanLeft(0)(_ + _).map(_ % LIST_LENGTH)
    val result = lengths.zip(starts).take(19).foldLeft[Seq[Int]](0 until LIST_LENGTH)((list, tuple) => {
      val (length, start) = tuple
      val result = wrapingReverse(list, start, length)
      result
    })
    println(result(0) * result(1))
  }

  def wrapingReverse[A](list: Seq[A], start: Int, length: Int): Seq[A] = {
      val total = list.length
      val list2 = list ++ list
      val reversed = list2.drop(start).take(length).reverse ++ list2.drop(start + length)
      reversed.take(total).drop(total - start) ++ reversed.take(total - start)
  }
}

