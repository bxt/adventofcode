package day24
import scala.io.Source
import scala.collection.mutable.{Map, HashMap}

object Main {
  case class Component(a: Int, b: Int) {
    def strength: Int = a + b
  }

  def strongestBridge(start: Int, components: List[Component]): Int = {
    if (components.size == 0) {
      0
    } else {
      components
        .map({ component =>
          if (component.a == start) {
            component.strength + strongestBridge(
              component.b,
              components.filter { _ != component }
            )
          } else if (component.b == start) {
            component.strength + strongestBridge(
              component.a,
              components.filter { _ != component }
            )
          } else {
            0
          }
        })
        .max
    }
  }

  def longestBridge(start: Int, components: List[Component]): (Int, Int) = {
    if (components.size == 0) {
      (0, 0)
    } else {
      components
        .map({ component =>
          if (component.a == start) {
            longestBridge(
              component.b,
              components.filter { _ != component }
            ) match {
              case (length, strength) =>
                (length + 1, strength + component.strength)
            }
          } else if (component.b == start) {
            longestBridge(
              component.a,
              components.filter { _ != component }
            ) match {
              case (length, strength) =>
                (length + 1, strength + component.strength)
            }
          } else {
            (0, 0)
          }
        })
        .max
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day24/input.txt").getLines()

    var components = input
      .map({ line =>
        val parts = line.split("/")
        Component(parts(0).toInt, parts(1).toInt)
      })
      .toList

    println(strongestBridge(0, components))

    println(longestBridge(0, components)._2)
  }
}
