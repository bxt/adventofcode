package day13

import scala.io.Source
import scala.collection.immutable.Map
import scala.annotation.tailrec

object Main {
  val lineRegex = raw"(\d+): (\d+)".r

  case class Scanner(depth: Int, range: Int) {
    def position(time: Int): Int = {
      maxpos - (maxpos - (time % (maxpos*2))).abs
    }

    def caught(delay: Int): Boolean = {
      position(delay + depth) == 0
    }

    def maxpos = range - 1
    def severity = depth * range
  }

  def main(args: Array[String]): Unit = {
    val example = List(0 -> 3, 1 -> 2, 4 -> 4, 6 -> 4).map(Scanner.tupled)
    assert((0 to 8).map(n => example.head.position(n)) == List(0, 1, 2, 1, 0, 1, 2, 1, 0))
    assert(example.map(_.caught(0)) == List(true, false, false, true))
    assert(sverity(example, 0) == 24)
    assert(goodDelay(example) == 10)

    val input = parse(Source.fromResource("day13/input.txt").getLines().toList)

    println(sverity(input, 0)) // 788
    println(goodDelay(input)) // 3905748
  }

  def sverity(scanners: Seq[Scanner], delay: Int): Int = {
    scanners.filter(_.caught(delay)).map(_.severity).sum
  }

  def goodDelay(scanners: Seq[Scanner]): Int = {
    Stream.from(0).find(delay => !scanners.exists(_.caught(delay))).get
  }

  def parse(input: Seq[String]): Seq[Scanner] = {
    input.map({ case lineRegex(fromStr, toStr) => {
      Scanner(fromStr.toInt, toStr.toInt)
    }})
  }
}

