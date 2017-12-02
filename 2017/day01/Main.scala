package day01

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day01/input.txt").mkString
    val digitList = toDigitList(input)

    List( "1122" -> 3
        , "1111" -> 4
        , "1234" -> 0
        , "91212129" -> 9
        ) foreach { case (input, output) =>
          assert(partOne(toDigitList(input)) == output)
          }

    println(partOne(digitList)) // 1216

    List( "1212" -> 6
        , "1221" -> 0
        , "123425" -> 4
        , "123123" -> 12
        , "12131415" -> 4
        ) foreach { case (input, output) =>
          assert(partTwo(toDigitList(input)) == output)
          }

    println(partTwo(digitList)) // 1072
  }

  def partOne(digitList: List[Int]): Int = {
    sumMatchingDigits(digitList)
  }

  def partTwo(digitList: List[Int]): Int = {
    sumMatchingDigits(digitList, digitList.length / 2)
  }

  def sumMatchingDigits(numbers: Seq[Int], offset: Int = 1): Int = {
    lazy val cycle: Stream[Int] = numbers.toStream #::: cycle
    val numberPairs = numbers.zip(cycle.drop(offset))
    numberPairs.map({case (a, b) => if (a == b) a else 0}).sum
  }

  def toDigitList(s: String): List[Int] = {
    s.map(_.asDigit).filter(_ >= 0).filter(_ < 10).toList
  }
}
