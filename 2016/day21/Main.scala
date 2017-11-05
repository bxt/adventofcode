import scala.io.Source

object Main {
  val swapPosition = "swap position (\\d+) with position (\\d+)".r
  val swapLetter = "swap letter ([a-z]) with letter ([a-z])".r
  val rotateLeft = "rotate left (\\d+) steps?".r
  val rotateRight = "rotate right (\\d+) steps?".r
  val specialRotate = "rotate based on position of letter ([a-z])".r
  val reverse = "reverse positions (\\d+) through (\\d+)".r
  val move = "move position (\\d+) to position (\\d+)".r

  def main(args: Array[String]): Unit = {
    println(scramble("abcdefgh"))
  }

  def scramble(input: String): String = {
    var s = input

    Source.fromResource("input.txt").getLines().foreach(line => {
      s = line match {
        case swapPosition(from, to) => swap_letter(s, s(from.toInt), s(to.toInt))
        case swapLetter(letter1, letter2) => swap_letter(s, letter1.head, letter2.head)
        case rotateLeft(steps) => rotate(s, steps.toInt)
        case rotateRight(steps) => rotate(s, s.length() - steps.toInt)
        case specialRotate(letter) => specialRotated(s, letter)
        case reverse(from, to) => reversed(s, from.toInt, to.toInt)
        case move(from, to) => moved(s, from.toInt, to.toInt)
      }
    });

    s
  }

  def swap_letter(input: String, letter1: Char, letter2: Char): String = {
    input.map(char => {
      char match {
        case `letter1` => letter2
        case `letter2` => letter1
        case anyLetter => anyLetter
      }
    });
  }

  def rotate(input: String, steps: Int): String = {
    input.drop(steps) ++ input.take(steps)
  }

  def reversed(input: String, from: Int, to: Int): String = {
    val length = to - from + 1
    input.patch(from, input.drop(from).take(length).reverse, length)
  }

  def moved(input: String, from: Int, to: Int): String = {
    val letter = input(from)
    input.patch(from, Nil, 1).patch(to, letter.toString, 0)
  }

  def specialRotated(input: String, letter: String): String = {
    val steps = input.indexOf(letter)
    rotate(input, input.length() - (steps + 1 + (if (steps >= 4) 1 else 0)) % input.length())
  }
}
