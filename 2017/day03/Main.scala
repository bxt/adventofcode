package day03

import scala.io.Source

// BTW: http://bernhardhaeussner.de/blog/26_Aufgewickelter_Zahlenstrahl

object Main {
  case class Point(x: Int, y: Int) {
    def +(that: Point) = that match {case Point(x2, y2) => Point(x + x2, y + y2)}
    def manhattanNorm(): Int = x.abs + y.abs
    def mooreNeighborhood(): List[Point] = Point.eightNeighboursOfZero.map(_ + this)
  }

  // Extends is workaround for this bug: https://issues.scala-lang.org/browse/SI-3664
  object Point extends ((Int, Int) => Point) {
    val zero = Point(0, 0)
    val eightNeighboursOfZero = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)).map(Point.tupled)
  }

  def main(args: Array[String]): Unit = {
    List(  9 -> ( 1,  1)
        , 10 -> ( 2,  1)
        , 11 -> ( 2,  0)
        , 13 -> ( 2, -2)
        , 14 -> ( 1, -2)
        , 17 -> (-2, -2)
        , 18 -> (-2, -1)
        , 21 -> (-2,  2)
        , 22 -> (-1,  2)
    ) foreach { case (input, output) =>
      assert(gridCoords(input) == Point.tupled(output))
    }

    val input = 312051
    println(partOne(input)) // 430
    println(partTwo(input)) // 312453
  }

  def partOne(input: Int): Int = {
    gridCoords(input).manhattanNorm
  }

  def partTwo(input: Int): Int = {
    ulamFibonaccis.find(_ > input).get
  }

  def ulamFibonaccis(): Stream[Int] = {
    val values = scala.collection.mutable.Map(Point.zero -> 1)
    Stream(1) #::: Stream.from(0).map(gridCoords).map(point => {
      val sum = point.mooreNeighborhood().map(n => values.getOrElse(n, 0)).sum
      values(point) = sum
      sum
    })
  }

  def gridCoords(input: Int): Point = {
    val sqrt = math.sqrt(input)
    val ring = math.floor(sqrt).toInt
    val halfRing = ring/2
    val resid = input - ring*ring

    if (isEven(ring)) {
      if (resid == 0) {
        Point(-halfRing + 1, -halfRing)
      } else if(resid <= ring) {
        Point(-halfRing, resid - halfRing - 1)
      } else {
        Point(-halfRing + resid - ring - 1, halfRing)
      }
    } else {
      if (resid == 0) {
        Point(halfRing, halfRing)
      } else if (resid <= ring) {
        Point(halfRing + 1, halfRing + 1 - resid)
      } else {
        Point(halfRing - resid + ring + 2, -halfRing - 1)
      }
    }
  }

  def isEven(number: Int) = number % 2 == 0
}
