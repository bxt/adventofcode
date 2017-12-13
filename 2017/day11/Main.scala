package day11

import scala.io.Source

/**
 * Axes:
 *
 * x   y   z
 * ^   ^   ^
 *  \  |  /
 *   \ | /
 *    \|/
 *
 */
object Main {
  case class Pos(x: Int, y: Int, z: Int) {
    def +(other: Pos): Pos = {
      Pos(x + other.x, y + other.y, z + other.z)
    }

    def distance = x.abs + y.abs + z.abs

    def normalize: Pos = {
           if (-z > 0 &&  y > 0) { val d = math.min(-z,  y); Pos(x + d, y - d, z + d) }
      else if ( x > 0 &&  z > 0) { val d = math.min( x,  z); Pos(x - d, y + d, z - d) }
      else if ( y > 0 && -x > 0) { val d = math.min( y, -x); Pos(x + d, y - d, z + d) }
      else if ( z > 0 && -y > 0) { val d = math.min( z, -y); Pos(x - d, y + d, z - d) }
      else if (-x > 0 && -z > 0) { val d = math.min(-x, -z); Pos(x + d, y - d, z + d) }
      else if (-y > 0 &&  x > 0) { val d = math.min(-y,  x); Pos(x - d, y + d, z - d) }
      else this
    }
  }

  object Pos {
    def zero = Pos(0, 0, 0)

    def direction(dir: String): Pos = dir match {
      case "n"  => Pos( 0,  1,  0)
      case "nw" => Pos( 1,  0,  0)
      case "ne" => Pos( 0,  0,  1)
      case "s"  => Pos( 0, -1,  0)
      case "sw" => Pos( 0,  0, -1)
      case "se" => Pos(-1,  0,  0)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day11/input.txt").getLines().next.split(',').toList

    val positions = input.scanLeft(Pos.zero)((pos, dir) => {
      (pos + Pos.direction(dir)).normalize
    })

    val distances = positions.map(_.distance)

    println(distances.last) // (-99, 308, 374) -> (0, 209, 473) -> 682
    println(distances.max) // 1406
  }
}

