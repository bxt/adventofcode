package day25
import scala.io.Source
import scala.collection.mutable.{ListBuffer}

object Main {
  val startStateRx = raw"Begin in state ([A-Z]).".r
  val interationsRx = raw"Perform a diagnostic checksum after (\d+) steps.".r
  val stateHeaderRx = raw"In state ([A-Z]):".r
  val stateValueRx = raw"  If the current value is ([0-1]):".r
  val stateNewValueRx = raw"    - Write the value ([0-1]).".r
  val stateDirectionRx = raw"    - Move one slot to the (right|left).".r
  val stateNextStateRx = raw"    - Continue with state ([A-Z]).".r

  class Band(
      private var left: List[String] = List("0"),
      private var right: List[String] = List.empty[String]
  ) {
    def value: String = left(0)
    def count: Int = left.count(_ == "1") + right.count(_ == "1")
    override def toString: String =
      s"... ${left.reverse.tail.mkString("  ")} [${left.head}] ${right.mkString("  ")} ..."

    def set(value: String) = {
      left = value :: left.tail
    }

    def move(direction: String) = direction match {
      case "left" => {
        right = left.head :: right
        left = left.tail
        if (left.isEmpty) {
          left = "0" :: left
        }
      }
      case "right" => {
        if (right.isEmpty) {
          left = "0" :: left
        } else {
          left = right.head :: left
          right = right.tail
        }
      }
    }

  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day25/input.txt").getLines()

    val startState = input.next() match {
      case startStateRx(letter) => letter
    }
    val interations = input.next() match {
      case interationsRx(numberString) => numberString.toInt
    }
    val transitions = new ListBuffer[(String, String, String, String, String)]()

    while (!input.isEmpty) {
      input.next() match {
        case stateHeaderRx(letter) => {
          1.to(2).foreach { _ =>
            (input.next(), input.next(), input.next(), input.next()) match {
              case (
                    stateValueRx(value),
                    stateNewValueRx(newValue),
                    stateDirectionRx(direction),
                    stateNextStateRx(nextState)
                  ) => {
                transitions += (
                  (
                    letter,
                    value,
                    newValue,
                    direction,
                    nextState
                  )
                )
              }
            }
          }
        }
        case _ => {}
      }
    }

    var state = startState
    var band = new Band()

    1.to(interations).foreach { _ =>
      val stableState = state
      val stableValue = band.value
      transitions.foreach {
        case (`stableState`, `stableValue`, newValue, direction, nextState) => {
          state = nextState
          band.set(newValue)
          band.move(direction)
        }
        case _ => {}
      }
    }

    println(band.count)
  }
}
