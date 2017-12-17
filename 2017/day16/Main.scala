package day16

import scala.io.Source

object Main {
  val spinRx = raw"s(\d+)".r
  val exchangeRx = raw"x(\d+)/(\d+)".r
  val partnerRx = raw"p([a-z])/([a-z])".r

  def main(args: Array[String]): Unit = {
    tests()

    val input = Source.fromResource("day16/input.txt").getLines().next
    val initial = 'a'.to('p').toList.mkString
    val theDance = dance(initial, input)
    val theDanceTwice = dance(theDance, input)
    println(theDance)
    println(theDanceTwice)
    println(repeat(theDanceTwice, 1000000000/2))

    val lll = Iterator.iterate(initial){ x => dance(x, input) }.take(20).toList
    val xxx = lll.drop(10).head
    val yyy = repeat(theDanceTwice, 5)
    println(lll)
    println(xxx)
    println(yyy)
    //assert(xxx == yyy)
  }

  def repeat(permutation: String, times: Int): String = {
    if (times % 2 == 0) {
      val repeated = repeat(permutation, times/2)
      chain(repeated, repeated)
    } else if (times % 5 == 0) {
      val repeated = repeat(permutation, times/5)
      Iterator.iterate(repeated){ x => chain(x, repeated) }.drop(4).next
    } else {
      val repeated = Iterator.iterate(permutation){ x => chain(x, permutation) }.drop(times - 1).next
      repeated
    }
  }

  def chain(permutation1: String, permutation2: String): String = {
    permutation2.map(x => permutation1(x - 'a'))
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
    input.map(_ match {
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
    val exampleDanceSwitches = "s1,x3/4"
    assert(dance("abcde", exampleDanceMoves) == "baedc")
    assert(dance("baedc", exampleDanceMoves) == "ceadb")
    assert(dance("abcde", exampleDanceMoves + "," + exampleDanceMoves) == "ceadb")
    assert(dance("abcde", exampleDanceSwitches + "," + exampleDanceSwitches) == "ceadb")
    assert(dance("abcde", List.fill(4)(exampleDanceMoves).mkString(",")) == chain("ceadb","ceadb"))
    assert(dance("abcde", List.fill(160)(exampleDanceMoves).mkString(",")) == repeat("ceadb", 80))
    assert(chain("acbd", "bcad") == "cbad")

    val pm1 = "nihjpcmfgaekblod"
    val pm2 = "lgfadhbcmnpeikoj"
    val pm3 = "kmcnjfihbldpgeoa"
    assert(chain(pm1, pm1) == pm2)
    assert(chain(pm1, pm2) == pm3)
    assert(chain(pm2, pm1) == pm3)
    assert(repeat(pm1, 1) == pm1)
    assert(repeat(pm1, 2) == pm2)
    assert(repeat(pm1, 3) == pm3)
    assert(repeat(pm1, 5) == chain(pm3, pm2))
  }
}
