package day22
import scala.io.Source
import scala.collection.mutable.{Map, HashMap}

object Main {
  val ITERATIONS = 10000

  case class Vec2(x: Int, y: Int) {
    def +(other: Vec2): Vec2 = {
      Vec2(x + other.x, y + other.y)
    }
    def turnLeft: Vec2 = {
      Vec2(y, -x)
    }
    def turnRight: Vec2 = {
      this.turnLeft.turnLeft.turnLeft // lol
    }
  }

  case class VirusCarrier(
      grid: Map[Vec2, Boolean],
      var position: Vec2,
      var velocity: Vec2
  ) {
    def burst: Boolean = {
      val isInfected = grid(this.position)
      val newIsInfected = !isInfected

      grid(this.position) = newIsInfected

      if (isInfected) {
        this.velocity = velocity.turnRight
      } else {
        this.velocity = velocity.turnLeft
      }

      this.position += this.velocity

      return newIsInfected
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day22/input.txt").getLines()
    var start = 0

    var grid = new HashMap[Vec2, Boolean]().withDefault(_ => false)

    input.zipWithIndex.foreach {
      case (line, y) => {
        start = line.length / 2
        line.zipWithIndex.foreach { case (letter, x) =>
          grid(Vec2(x, y)) = letter == '#'
        }
      }
    }

    val virusCarrier = VirusCarrier(grid, Vec2(start, start), Vec2(0, -1))

    var count: Int = 0

    1.to(ITERATIONS).foreach { _ =>
      if (virusCarrier.burst) {
        count += 1
      }
    }

    println(count)
  }
}
