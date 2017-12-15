package day15

import scala.annotation.tailrec

object Main {
  type Gen = Iterator[Long]

  val MASK: Long = (1 << 16) - 1
  val MODULUS = 2147483647
  val aFactor = 16807
  val bFactor = 48271
  val aMask = 4 - 1
  val bMask = 8 - 1

  def main(args: Array[String]): Unit = {
    val (a, b) = generators(65, 8921)
    assert(a.take(5).toList == List(1092455, 1181022009, 245556042, 1744312007, 1352636452))
    assert(b.take(5).toList == List(430625591, 1233683848, 1431495498, 137874439, 285222916))
    assert(!matches(1092455, 430625591))
    assert(matches(245556042, 1431495498))
    assert(part1(65, 8921) == 588)
    assert(1L.to(20).filter(meets(aMask)).toList == List(4, 8, 12, 16, 20))
    assert(1L.to(40).filter(meets(bMask)).toList == List(8, 16, 24, 32, 40))
    assert(part2(65, 8921) == 309)

    println(part1(289, 629)) // 638
    println(part2(289, 629)) // 343
  }

  def part1(aStart: Int, bStart: Int): Int = {
    val (a, b) = generators(aStart, bStart)
    countMatches(a, b, 40000000)
  }

  def part2(aStart: Int, bStart: Int): Int = {
    val (a, b) = generators(aStart, bStart)
    countMatches(a.filter(meets(aMask)), b.filter(meets(bMask)), 5000000)
  }

  def generators(aStart: Int, bStart: Int): (Gen, Gen) = {
    (generator(aStart, aFactor, MODULUS), generator(bStart, bFactor, MODULUS))
  }

  def countMatches(a: Gen, b: Gen, until: Int): Int = {
    a.zip(b).take(until).count((matches _).tupled)
  }

  def generator(start: Long, factor: Long, modulus: Long): Gen = {
    Iterator.iterate(start) { prev => prev * factor % modulus }.drop(1)
  }

  def matches(a: Long, b: Long): Boolean = {
    (a & MASK) == (b & MASK)
  }

  def meets(mask: Long)(a: Long): Boolean = {
    (a & mask) == 0
  }
}
