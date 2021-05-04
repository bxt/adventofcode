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

  def bestBridgeValue[R: Ordering](
      start: Int,
      components: List[Component],
      default: R,
      combine: (R, Component) => R
  ): R = {
    if (components.size == 0) {
      default
    } else {
      components
        .map({ component =>
          component
            .counterpart(start)
            .map { counterpart =>
              combine(
                bestBridgeValue(
                  counterpart,
                  components.filter { _ != component },
                  default,
                  combine
                ),
                component
              )
            }
            .getOrElse { default }
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

    println(
      bestBridgeValue[Int](
        0,
        components,
        0,
        { (strength, component) => strength + component.strength }
      )
    )

    println(
      bestBridgeValue[(Int, Int)](
        0,
        components,
        (0, 0),
        { case ((length, strength), component) =>
          (length + 1, strength + component.strength)
        }
      )._2
    )
  }
}
