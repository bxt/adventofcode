package day14

import scala.io.Source
import scala.collection.immutable.Map
import scala.annotation.tailrec

import day03.Main.Point
import day03.Main.Point.VonNeumann

import day10.Main.knotHash

import day12.Main.connectedComponents

object Main {
  case class Grid(values: Seq[Seq[Boolean]]) {
    def count: Int = values.map(bin => bin.count(identity)).sum

    def pointsAndValues: Seq[Seq[(Point, Boolean)]] = {
      values.zipWithIndex.map({
        case (rowValues, row) => {
          rowValues.zipWithIndex.map({ case (value, col) => (Point(row, col), value) })
        }
      })
    }

    def points: Set[Point] = pointsAndValues.flatten.filter(_._2).map(_._1).toSet

    def components: Set[Set[Point]] = {
      val points = this.points
      val links = points.flatMap(p => (p.neighborhood(VonNeumann).toSet & points | Set(p)).map(n => (p, n)))
      connectedComponents(links)
    }

    def mkString: String = {
      val ccSeq = components.toSeq

      pointsAndValues.map(
        _.map({
          case (p, value) => if (value) {
            val ccNum: BigInt = ccSeq.indexWhere(_(p))
            ccNum.toString(36).padTo(3, ' ')
          } else " . "
        }).mkString).mkString("\n")
    }
  }

  object Grid {
    val SIZE = 128

    def generate(input: String): Grid = {
      Grid(0.until(SIZE)
        .map(n => f"$input%s-$n%d")
        .map(knotHash(_))
        .map(h => BigInt(h, 16).toString(2))
        .map(binStr => binStr.map({ case '0' => false; case '1' => true }))
        .map(binStr => binStr.reverse.padTo(SIZE, false).reverse))
    }
  }

  def main(args: Array[String]): Unit = {
    val exampleGrid = Grid.generate("flqrgnkx")
    assert(exampleGrid.count == 8108)
    assert(exampleGrid.components.size == 1242)

    val input = "ljoxqyyw"
    val inputGrid = Grid.generate(input)
    println(inputGrid.count) // 8316
    println(inputGrid.components.size) // 1074
    println(inputGrid.mkString)
  }
}

