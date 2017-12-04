package day04

import scala.io.Source

object Main {
   def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day04/input.txt").getLines()

    val parsedInput = input.map(line => {
      line.split("\\s+")
    }).toList

    println(parsedInput.filter(words => {
      words.length == words.toSet.size
    }).length) // 325

    println(parsedInput.filter(words => {
      words.length == words.map(_.toCharArray.sorted.mkString).toSet.size
    }).length) // 119
  }
}
