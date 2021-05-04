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

  abstract class BestBridgeFinder[R: Ordering] {
    def zero: R
    def combine(r: R, component: Component): R
    def bestBridgeValue(
        start: Int,
        components: List[Component]
    ): R = {
      if (components.size == 0) {
        zero
      } else {
        components
          .map({ component =>
            component
              .counterpart(start)
              .map { counterpart =>
                combine(
                  bestBridgeValue(
                    counterpart,
                    components.filter { _ != component }
                  ),
                  component
                )
              }
              .getOrElse { zero }
          })
          .max
      }
    }
  }

  class StrongestBridgeFinder extends BestBridgeFinder[Int] {
    override def zero = 0
    override def combine(r: Int, component: Component): Int =
      r + component.strength
  }

  class LongestAndStrongestBridgeFinder extends BestBridgeFinder[(Int, Int)] {
    override def zero = (0, 0)
    override def combine(r: (Int, Int), component: Component): (Int, Int) =
      r match {
        case (length, strength) =>
          (length + 1, strength + component.strength)
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
      new StrongestBridgeFinder().bestBridgeValue(0, components)
    )

    println(
      new LongestAndStrongestBridgeFinder().bestBridgeValue(0, components)._2
    )
  }
}
