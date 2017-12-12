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
  type Pos = (Int, Int, Int)

  def main(args: Array[String]): Unit = {

    val input = Source.fromResource("day11/input.txt").getLines().next.split(',').toList

    val positions = input.scanLeft((0, 0, 0))((pos, dir) => {
      normalize(add(pos, dirVector(dir)))
    })

    val distances = positions.map(distance)

    println(distances.last) // (-99, 308, 374) -> (0, 209, 473) -> 682
    println(distances.max) // 1406
  }

  def dirVector(dir: String): Pos = dir match {
    case "n"  => ( 0,  1,  0)
    case "nw" => ( 1,  0,  0)
    case "ne" => ( 0,  0,  1)
    case "s"  => ( 0, -1,  0)
    case "sw" => ( 0,  0, -1)
    case "se" => (-1,  0,  0)
  }

  def add(pos1: Pos, pos2: Pos): Pos = {
      val (x1, y1, z1) = pos1
      val (x2, y2, z2) = pos2
      (x1 + x2, y1 + y2, z1 + z2)
  }

  def normalize(pos: Pos): Pos = {
    val (x, y, z) = pos
         if (-z > 0 &&  y > 0) { val d = math.min(-z,  y); (x + d, y - d, z + d) }
    else if ( x > 0 &&  z > 0) { val d = math.min( x,  z); (x - d, y + d, z - d) }
    else if ( y > 0 && -x > 0) { val d = math.min( y, -x); (x + d, y - d, z + d) }
    else if ( z > 0 && -y > 0) { val d = math.min( z, -y); (x - d, y + d, z - d) }
    else if (-x > 0 && -z > 0) { val d = math.min(-x, -z); (x + d, y - d, z + d) }
    else if (-y > 0 &&  x > 0) { val d = math.min(-y,  x); (x - d, y + d, z - d) }
    else pos
  }

  def distance(pos: Pos): Int = {
    val (x, y, z) = pos
    x.abs + y.abs + z.abs
  }
}

