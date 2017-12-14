package day14

import scala.io.Source
import scala.collection.immutable.Map
import scala.annotation.tailrec

import day03.Main.Point
import day03.Main.Point.VonNeumann

object Main {
  type Grid = Seq[Seq[Boolean]]

  val GRID_SIZE = 128

  def main(args: Array[String]): Unit = {
    val example = "flqrgnkx"
    val exampleGrid = generateGrid(example)
    //printGrid(exampleGrid)
    val exampleGridIndices = gridPoints(exampleGrid)
    assert(exampleGridIndices(Point(0, 0)))
    assert(exampleGridIndices(Point(0, 1)))
    assert(!exampleGridIndices(Point(1, 0)))
    assert(exampleGridIndices(Point(1, 1)))
    assert(countGrid(exampleGrid) == 8108)
    println("Well..?")
    val ccs = gridCcs(exampleGrid)
    printGridWithCcs(exampleGrid, ccs)
    assert(ccs.size == 1242)

    val input = "ljoxqyyw"
    val inputGrid = generateGrid(input)
    println(countGrid(inputGrid))
    println(gridCcs(inputGrid).size)
  }

  def generateGrid(input: String): Grid = {
    0
        .until(GRID_SIZE)
        .map(n => f"$input%s-$n%d")
        .map(day10.Main.knotHash(_))
        .map(h => BigInt(h, 16).toString(2))
        .map(binStr => binStr.map({ case '0' => false; case '1' => true}).reverse.padTo(GRID_SIZE, false).reverse)
  }

  def printGrid(grid: Grid): Unit = {
    println(grid.map(bin => bin.map(x => if (x) '#' else '.').mkString).mkString("\n"))
  }

  def printGridWithCcs(grid: Grid, ccs: Set[Set[Point]]): Unit = {
    val ccSeq = ccs.toSeq

    println(grid.zipWithIndex.map({ case (rowValues, row) =>
      rowValues.zipWithIndex.map({ case (value, col) => if (value) {
        val ccNum = ccSeq.indexWhere(_(Point(row, col)))
        val x: BigInt = ccNum
        x.toString(36).padTo(3, ' ')
      } else " . "}).mkString
    }).mkString("\n"))
  }

  def countGrid(grid: Grid): Int = {
    grid.map(bin => bin.count(identity)).sum
  }

  def gridPoints(grid: Grid): Set[Point] = {
    grid.zipWithIndex.flatMap({ case (rowValues, row) =>
      rowValues.zipWithIndex.flatMap({ case (value, col) => if (value) Some(Point(row, col)) else None })
    }).toSet
  }

  def gridCcs(grid: Grid): Set[Set[Point]] = {
    val points = gridPoints(grid)
    val links = points.flatMap(p => (p.neighborhood(VonNeumann).toSet & points | Set(p)).map(n => (p, n)))
    println(links.filter(ps => ps._1.x < 5 && ps._1.y < 5 && ps._2.x < 5 && ps._2.y < 5 ))
    day12.Main.connectedComponents(links)
  }
}

