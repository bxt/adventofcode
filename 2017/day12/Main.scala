package day12

import scala.io.Source
import scala.collection.immutable.Map
import scala.annotation.tailrec

object Main {
  val lineRegex = raw"(\d+) <-> ([\d, ]+)".r

  def main(args: Array[String]): Unit = {
    val example = parse(List("0 <-> 2", "1 <-> 1", "2 <-> 0, 3, 4", "3 <-> 2, 4", "4 <-> 2, 3, 6", "5 <-> 6", "6 <-> 4, 5"))
    assert(connectedComponent(example, List(1)) == Set(1))
    assert(connectedComponent(example, List(0)) == Set(0, 2, 3, 4, 5, 6))

    val input = parse(Source.fromResource("day12/input.txt").getLines().toList)

    println(connectedComponent(input, List(0)).size) // 288
    println(connectedComponents(input).size) // 211
  }

  def parse(input: Seq[String]): Set[(Int, Int)] = {
    val links = input.flatMap({ case lineRegex(fromStr, toStrs) => {
        toStrs.split(",\\s*").map(_.toInt).map(x => (fromStr.toInt, x))
    }})

    val symmetric = links.flatMap(_ match { case (x, y) => List((x, y), (y, x)) })

    symmetric.toSet
  }

  def connectedComponents(links: Set[(Int, Int)]): Set[Set[Int]] = {
    links.map(_._1).toList.foldLeft((Set[Int](), Set[Set[Int]]()))((t, x) => {
      val (seen, ccs) = t
      if (seen.contains(x)) t
      else {
        val newCc = connectedComponent(links, List(x))
        (seen | newCc, ccs + newCc)
      }
    })._2
  }

  @tailrec
  def connectedComponent(links: Set[(Int, Int)], queue: Seq[Int], seen: Set[Int] = Set()): Set[Int] = queue match {
    case List() => seen
    case x +: xs => {
      val additional = links.filter(_._1 == x).map(_._2).toSet &~ seen
      connectedComponent(links, additional.toList ++ xs, seen | additional)
    }
  }
}

