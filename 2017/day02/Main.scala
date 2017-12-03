package day02

import scala.io.Source

object Main {
   def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day02/input.txt").getLines()

    val parsedInput = input.map(line => {
      line.split("\\s+").map(_.toInt)
    }).toList

    println(parsedInput.map(xs => xs.max - xs.min).sum) // 50376

    println(parsedInput.map(xs => {
      xs.combinations(2)
        .find(pair => pair.max % pair.min == 0)
        .map(pair => pair.max / pair.min)
        .get
    }).sum) // 267
  }
}
