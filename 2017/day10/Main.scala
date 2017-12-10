package day10

import scala.io.Source

object Main {
  val ROUNDS = 64
  val SPARSE_GROUP_SIZE = 16
  val LIST_LENGTH = 256
  val LENGTHS_SUFFIX = List(17, 31, 73, 47, 23)

  def main(args: Array[String]): Unit = {
    assert(wrapingReverse(List(0, 1, 2, 3, 4), 0, 3) == List(2, 1, 0, 3, 4))
    assert(wrapingReverse(List(2, 1, 0, 3, 4), 3, 4) == List(4, 3, 0, 1, 2))
    assert(wrapingReverse(List(4, 3, 0, 1, 2), 3, 1) == List(4, 3, 0, 1, 2))
    assert(wrapingReverse(List(4, 3, 0, 1, 2), 1, 5) == List(3, 4, 2, 1, 0))

    assert("1,2,3".toList == List(49, 44, 50, 44, 51))
    assert(densify(List(65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22)) == List(64))

    assert(knotHash("")         == "a2582a3a0e66e6e86e3812dcb672a272")
    assert(knotHash("AoC 2017") == "33efeb34ea91902bb2f59c9920caa6cd")
    assert(knotHash("1,2,3")    == "3efbe78a8d82f29979031a4aa0b16a9d")
    assert(knotHash("1,2,4")    == "63960835bcdc130f0b66d7ff4f6a5a8e")

    val input = Source.fromResource("day10/input.txt").getLines().next
    println(part1(input)) // 826
    println(part2(input)) // d067d3f14d07e09c2e7308c3926605c4
  }

  def part1(input: String): Int = {
    val lengths = input.split(",").map(_.toInt).toList
    val resultPart1 = runReverses(lengths)
    resultPart1(0) * resultPart1(1)
  }

  def part2 = knotHash _

  def knotHash(input: String): String = {
    val lengths = input.toList.map(_.toInt) ++ LENGTHS_SUFFIX
    val repeatedLengths = List.fill(ROUNDS)(lengths).flatten
    val sparseHash = runReverses(repeatedLengths)
    val denseHash = densify(sparseHash)
    denseHash.map(n => f"$n%02x").mkString
  }

  def densify(sparse: Seq[Int]): Seq[Int] = {
    sparse.grouped(SPARSE_GROUP_SIZE).map(_.reduce(_ ^ _)).toList
  }

  def runReverses(lengths: Seq[Int]): Seq[Int] = {
    val starts = lengths.zipWithIndex.map(x => x._1 + x._2).scanLeft(0)(_ + _).map(_ % LIST_LENGTH)
    lengths.zip(starts).foldLeft[Seq[Int]](0 until LIST_LENGTH)((list, tuple) => {
      val (length, start) = tuple
      wrapingReverse(list, start, length)
    })
  }

  def wrapingReverse[A](list: Seq[A], start: Int, length: Int): Seq[A] = {
    val total = list.length
    val list2 = list ++ list
    val reversed = list2.drop(start).take(length).reverse ++ list2.drop(start + length)
    reversed.take(total).drop(total - start) ++ reversed.take(total - start)
  }
}

