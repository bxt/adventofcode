package day24
import scala.io.Source
import scala.collection.mutable.{Map, HashMap}

object Main {
  case class Component(a: Int, b: Int) {
    def strength: Int = a + b
    def counterpart(start: Int): Option[Int] = start match {
      case `a` => Some(b)
      case `b` => Some(a)
      case _   => None
    }
  }

  def strongestBridge(start: Int, components: List[Component]): Int = {
    if (components.size == 0) {
      0
    } else {
      components
        .map({ component =>
          component
            .counterpart(start)
            .map { counterpart =>
              component.strength + strongestBridge(
                counterpart,
                components.filter { _ != component }
              )
            }
            .getOrElse { 0 }
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
          component
            .counterpart(start)
            .map { counterpart =>
              longestBridge(
                counterpart,
                components.filter { _ != component }
              ) match {
                case (length, strength) =>
                  (length + 1, strength + component.strength)
              }
            }
            .getOrElse { (0, 0) }
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
