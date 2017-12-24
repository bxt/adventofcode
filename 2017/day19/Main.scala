package day19

import scala.io.Source

import day03.Main.Point
import day03.Main.Point.VonNeumann
import scala.collection.GenTraversable
import scala.annotation.tailrec

object Main {
  val input = Source.fromResource("day19/input.txt").getLines().toList

  sealed trait CharType
  case object Turn extends CharType
  case object Space extends CharType
  case class Other(char: Char) extends CharType

  case class State(position: Point, heading: Point) {
    def next: Option[State] = {
      charType match {
        case Space => None
        case Turn => position
          .neighborhood(VonNeumann)
          .filterNot(_ == position + heading)
          .filterNot(_ == position - heading)
          .map(p => State(p, p - position))
          .find(s => s.isPathway)
        case Other(_) => Some(copy(position = position + heading))
      }
    }

    def char: Char = input.applyOrElse(position.x, Function.const("")).applyOrElse(position.y, Function.const(' '))

    def charType: CharType = char match {
      case ' '       => Space
      case '+'       => Turn
      case other     => Other(other)
    }

    def isPathway = charType match {
      case Other(_) => true
      case _ => false
    }

    def isGoal = charType match {
      case Other(char) => !"-|".contains(char)
      case _ => false
    }
  }

  def main(args: Array[String]): Unit = {
    val downwards = Point(1, 0)
    val start = input.head.zipWithIndex.map({case (_, i) => State(Point(0, i), downwards)}).find(_.isPathway).get

    var steps = unfoldr(start)(_.next.map(x => (x, x)))

    println(steps.filter(_.isGoal).map(_.char).mkString)
    println(steps.length)
  }

  def unfoldr[A, B](start: B)(f: B => Option[(A, B)]): Stream[A] = f(start) match {
    case None => Stream.empty
    case Some((elem, next)) => elem #:: unfoldr(next)(f)
  }
}
