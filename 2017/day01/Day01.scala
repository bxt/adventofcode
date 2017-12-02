import scala.io.Source
import scala.util.Try

// 12539 too high

object Day01 {
  def main(args: Array[String]): Unit = {
    
    println(sumMatchingDigits(toDigitList("1122").map(_.toString.toInt))) // 3
    println(sumMatchingDigits(toDigitList("1111").map(_.toString.toInt))) // 4
    println(sumMatchingDigits(toDigitList("1234").map(_.toString.toInt))) // 0
    println(sumMatchingDigits(toDigitList("91212129").map(_.toString.toInt))) // 9
    
    val input = Source.fromResource("input.txt").mkString
    val digitList = toDigitList(input)
    println(sumMatchingDigits(digitList)) // 1216
  }
  
  def sumMatchingDigits(numbers: Seq[Int]): Int = {
    val numberPairs = (numbers :+ numbers.head).iterator.sliding(2)
    numberPairs.map({case a +: b +: _ => if (a == b) a else 0}).sum
  }
  
  def toDigitList(s: String): List[Int] = s.map(_.asDigit).filter(_ >= 0).filter(_ < 10).toList
}