package day22
import scala.io.Source
import scala.collection.mutable.{Map, HashMap}

object Main {
  case class Vec2(x: Int, y: Int) {
    def +(other: Vec2): Vec2 = {
      Vec2(x + other.x, y + other.y)
    }
    def turnLeft: Vec2 = {
      Vec2(y, -x)
    }
    def turnRight: Vec2 = {
      this.turnLeft.turnLeft.turnLeft
    }
    def reverse: Vec2 = {
      this.turnLeft.turnLeft
    }
  }

  class VirusCarrier(
      val grid: Map[Vec2, Int],
      var position: Vec2,
      var velocity: Vec2
  ) {
    def infectionSteps = 2

    def countInfections(iterations: Int): Int = {
      var count: Int = 0

      1.to(iterations).foreach { _ =>
        if (burst) {
          count += 1
        }
      }

      return count
    }

    def burst: Boolean = {
      val infectionState = grid(this.position)
      val newInfectionState = (infectionState + infectionSteps) % 4

      grid(this.position) = newInfectionState

      this.velocity = infectionState match {
        case 0 => velocity.turnLeft
        case 1 => velocity
        case 2 => velocity.turnRight
        case 3 => velocity.reverse
      }

      this.position += this.velocity

      return newInfectionState == 2
    }
  }

  class UpgradedVirusCarrier(
      grid: Map[Vec2, Int],
      position: Vec2,
      velocity: Vec2
  ) extends VirusCarrier(grid, position, velocity) {
    override def infectionSteps = 1
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

    var grid2 = new HashMap[Vec2, Int]().withDefault(_ => 0)
    grid2 ++= grid

    val virusCarrier = new VirusCarrier(grid, Vec2(start, start), Vec2(0, -1))

    println(virusCarrier.countInfections(10000))

    val upgradedVirusCarrier =
      new UpgradedVirusCarrier(grid2, Vec2(start, start), Vec2(0, -1))

    println(upgradedVirusCarrier.countInfections(10000000))
  }
}
