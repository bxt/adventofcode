package day25
import scala.io.Source
import scala.collection.mutable.{Map, HashMap, ListBuffer}

object Main {
  val startStateRx = raw"Begin in state ([A-Z]).".r
  val interationsRx = raw"Perform a diagnostic checksum after (\d+) steps.".r
  val stateHeaderRx = raw"In state ([A-Z]):".r
  val stateValueRx = raw"  If the current value is ([0-1]):".r
  val stateNewValueRx = raw"    - Write the value ([0-1]).".r
  val stateDirectionRx = raw"    - Move one slot to the (right|left).".r
  val stateNextStateRx = raw"    - Continue with state ([A-Z]).".r

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

    println(startState)
    println(interations)
    println(transitions)
  }
}
