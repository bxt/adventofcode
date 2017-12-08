package day07

import scala.io.Source
import scala.reflect.ClassTag
import scala.annotation.tailrec
import scala.collection.mutable.HashMap

object Main {
  type Program = (String, Int, Array[String])

  val programRegex = raw"([a-z]+) \((\d+)\)( -> ([a-z, ]+))?".r

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day07/input.txt").getLines().map(parse).toList

    val bottom = input.filter({ case (name, _, _) =>
      !input.exists({ case (_, _, childrenNames) => childrenNames.contains(name)})
     }).head

    println(bottom._1) // aapssr

    lazy val weighter: String => Int = memoize { name: String => {
      val program = input.find(_._1 == name).get
      program._2 + program._3.map(weighter).sum
    }}

    val weightCorrections = input.flatMap({ case (name, _, childrenNames) =>
      val balance = childrenNames.groupBy(weighter)
      if (balance.keySet.size > 1) {
        val (badWeight, badsNames) = balance.minBy(_._2.length)
        val (goodWeight, _) = balance.maxBy(_._2.length)
        val diff = goodWeight - badWeight
        val badProgramm = input.find({ case (name, _, _) => name == badsNames.head}).get
        Some(badWeight, badProgramm._2 + diff)
      } else None
    })

    println(weightCorrections.minBy(_._1)._2) // 1458
  }

  // https://stackoverflow.com/a/36960228
  def memoize[I, O](f: I => O): I => O = new HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }

  def parse(input: String): Program = input match {
    case(programRegex(name, weightStr, _, null)) => (name, weightStr.toInt, Array())
    case(programRegex(name, weightStr, _, childrenStr)) => (name, weightStr.toInt, childrenStr.split(", +"))
  }
}
