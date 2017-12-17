package day16

import scala.io.Source

import day06.Main.findLooop

object Main {
  val spinRx = raw"s(\d+)".r
  val exchangeRx = raw"x(\d+)/(\d+)".r
  val partnerRx = raw"p([a-z])/([a-z])".r

  def main(args: Array[String]): Unit = {
    tests()

    val input = Source.fromResource("day16/input.txt").getLines().next
    val initial = 'a'.to('p').toList.mkString
    val theDance = dance(initial, input)
    println(theDance) // giadhmkpcnbfjelo

    val dances = Stream.iterate(initial){ x => dance(x, input) }
    val (index, loopSize) = findLooop(dances)
    val rest = (1000000000 - index) % loopSize
    println(dances.drop(index).drop(rest).head) // njfgilbkcoemhpad
  }

  def dance(initial: String, input: String): String = {
    val instructions = input.split(',').map(parse _)
    instructions.foldLeft(initial)((x, y) => y(x))
  }

  def parse(input: String): String => String = input match {
    case spinRx(stepsStr) => rotate(-stepsStr.toInt)
    case exchangeRx(pos1Str, pos2Str) => s => swapLetter(s(pos1Str.toInt), s(pos2Str.toInt))(s)
    case partnerRx(a, b) => swapLetter(a.head, b.head)
  }

  def swapLetter(letter1: Char, letter2: Char)(input: String): String = {
    input.map({
      case `letter1` => letter2
      case `letter2` => letter1
      case anyLetter => anyLetter
    });
  }

  def rotate(steps: Int)(input: String): String = {
    val size = input.length()
    val normalizedSteps = ((steps % size) + size) % size
    input.drop(normalizedSteps) ++ input.take(normalizedSteps)
  }

  def tests() {
    assert(parse("s1")("abcde") == "eabcd")
    assert(parse("x3/4")("eabcd") == "eabdc")
    assert(parse("pe/b")("eabdc") == "baedc")
    val exampleDanceMoves = "s1,x3/4,pe/b"
    assert(dance("abcde", exampleDanceMoves) == "baedc")
    assert(dance("baedc", exampleDanceMoves) == "ceadb")
    assert(dance("abcde", exampleDanceMoves + "," + exampleDanceMoves) == "ceadb")
  }
}
