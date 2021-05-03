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

 def longestBridges(start: Int, components: List[Component]): List[List[Component]] = {
  List.concat(List(List()), components
    .flatMap({ component =>
      (if (component.a == start) {
        longestBridges(
          component.b,
          components.filter { _ != component }
        )
      } else if (component.b == start) {
        longestBridges(
          component.a,
          components.filter { _ != component }
        )
      } else {
        List()
      }).map{list => List.concat(List(component), list)}
    }))
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

    println(longestBridges(0, components))
  }
}
