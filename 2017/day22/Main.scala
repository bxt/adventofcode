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
      grid: Map[Vec2, Int],
      var position: Vec2,
      var velocity: Vec2
  ) {
    def burst: Boolean = {
      val infectionState = grid(this.position)
      val newInfectionState = (infectionState + 2) % 4

      grid(this.position) = newInfectionState

      this.velocity = infectionState match {
        case 0 => velocity.turnLeft
        case 2 => velocity.turnRight
      }

      this.position += this.velocity

      return newInfectionState == 2
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day22/input.txt").getLines()
    var start = 0

    var grid = new HashMap[Vec2, Int]().withDefault(_ => 0)

    input.zipWithIndex.foreach {
      case (line, y) => {
        start = line.length / 2
        line.zipWithIndex.foreach { case (letter, x) =>
          grid(Vec2(x, y)) = if (letter == '#') 2 else 0
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
