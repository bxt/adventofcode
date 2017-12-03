package day03

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    assert(gridCoords(16) == (-1, -2))
    assert(gridCoords(17) == (-2, -2))
    assert(gridCoords(18) == (-2, -1))
    assert(gridCoords(21) == (-2, 2))
    assert(gridCoords(22) == (-1, 2))

    val input = 312051
    println(partOne(input))
  }

  def partOne(input: Int): Int = {
    manhattanNorm(gridCoords(input))
  }

  def gridCoords(input: Int): (Int, Int) = {
    val sqrt = math.sqrt(input)
    val ring = math.floor(sqrt).toInt
    val halfRing = ring/2
    val resid = input - ring*ring
    if (isEven(ring)) {
      if (resid == 0) {
        (-halfRing + 1, -halfRing)
      } else {
        if(resid <= ring) {
          (-halfRing, resid - halfRing - 1)
        } else {
          (-halfRing + resid - ring - 1, halfRing)
        }
      }
    } else {
      throw new NotImplementedError
    }
  }

  def isEven(number: Int) = number % 2 == 0

  def manhattanNorm(t: (Int, Int)) = t match { case (a, b) => a.abs + b.abs }
}
